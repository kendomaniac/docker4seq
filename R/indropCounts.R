#' @title A function to handle a indrop V2 single cell data
#' @description This function executes a docker that produces as output the sinngle cell counts from V2 indrop single cell sequencing
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param fastq.folder, a character string indicating the folder where input data are located and where output will be written
#' @param index.folder, a character string indicating the folder where transcriptome index was created with salmonIndex.
#' @author Raffaele Calogero and Riccardo Panero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @examples
#' \dontrun{
#' library(docker4seq)
#' #running salmonCounts
#' }
#'
#' @export
indropCounts <- function(group=c("sudo","docker"), scratch.folder, fastq.folder, index.folder){

  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #storing the position of the home folder
  home <- getwd()
  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(fastq.folder)){
    cat(paste("\nIt seems that the ",fastq.folder, " folder does not exist\n"))
    return(2)
  }
  setwd(fastq.folder)
  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(fastq.folder,"/tempFolderID", sep=""))
  project.folder <- scrat_tmp.folder
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  dir.create(paste(file.path(scrat_tmp.folder), "/input", sep=""))
  input.folder <- paste(file.path(scrat_tmp.folder), "/input", sep="")
  dir.create(paste(file.path(scrat_tmp.folder), "/output", sep=""))
  oputput.folder <- paste(file.path(scrat_tmp.folder), "/output", sep="")
  dir <- dir()
  dir <- dir[grep(".fastq.gz", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", fastq.folder, "there are not fastq.gz files"))
    return(1)
  }
  system(paste("chmod 777 -R", file.path(scrat_tmp.folder)))
  for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",paste(file.path(scrat_tmp.folder), "/input", sep=""),"/",i, sep=""))
  }

  yaml.file=paste(path.package(package="docker4seq"),"data/indrop.yaml",sep="/")
  system(paste("cp ",yaml.file," ", file.path(scrat_tmp.folder),sep=""))
  system(paste("chmod 777 -R", file.path(scrat_tmp.folder)))

  cat("\nsetting as working dir the scratch folder and running  docker container\n")

  params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", project.folder,":/data/scratch -v ",index.folder,":/index -d docker.io/repbioinfo/indrop.2017.01 sh /bin/indrop.sh ", sep="")
  resultRun <- runDocker(group=group,container="docker.io/repbioinfo/indrop.2017.01", params=params)




}
