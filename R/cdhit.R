#' @title A function to execute CD-HIT
#' @description This function executes a ubuntu docker that cluster minION sequences using CD-HIT
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param identity.threshold, sequence identity threshold, default 0.9, this is the default cd-hit's global sequence identity calculated as: number of identical bases in alignment divided by the full length of the shorter sequence
#' @param memory.limit, memory limit in MB for the program, default 30000. 0 for unlimitted
#' @param threads, number of threads, default 0; with 0, all CPUs will be used
#' @param word.length, 7 for thresholds between 0.88 and 0.9 for other option see user manual cdhit 
#' 
#' @author Raffaele A Calogero, raffaele.calogero [at] unito [dot] it, University of Torino. Italy
#'
#' @return Returns two files: a fasta file of representative sequences and a text file of list of clusters
#' @examples
#' \dontrun{
#'     #running fastq2fasta
#'     cdhit(group="docker", scratch.folder="/data/scratch", data.folder=getwd(), identity.threshold=0.90, memory.limit=8000, threads=0, word.length=7)
#' }
#'
#' @export
cdhit <- function(group=c("sudo","docker"), scratch.folder, data.folder, identity.threshold=0.90, memory.limit=30000, threads=0, word.length=7){

  
  output="output"
#running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }
  
  #storing the position of the home folder  
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1") 
    setwd(home)
    return(10)
  }
  

  
  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  
  ### copying fastq files 
  dir <- dir(data.folder)
  dir <- dir[grep(".fasta.gz$", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", data.folder, "there are not fasta files"))
    system("echo 1 >& ExitStatusFile")
    setwd(home)
    return(1)
  }else if(length(dir)>1){
    cat(paste("It seems that in ", data.folder, "there is more than 1 fasta files"))
    system("echo 2 >& ExitStatusFile")
    setwd(home)
    return(2)
  }else{
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",data.folder,"/",dir, " ",scratch.folder,"/",tmp.folder,"/",dir, sep=""))
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }
  ###
  
  #executing the docker job
  params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/repbioinfo/cdhit.2019.01:cdhit-V4.8.1 sh /bin/cdhit_est.sh ", dir, " ", identity.threshold, " ", memory.limit, " ", threads, " ", word.length, " ", output, sep="")
  resultRun <- runDocker(group=group, params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nCDHIT analysis is finished is finished\n")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("cdhit user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("cdhit system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("cdhit elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("cdhit run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("cdhit system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("cdhit elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),"_cdhit.log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
