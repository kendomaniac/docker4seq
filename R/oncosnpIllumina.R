#' @title A function to handle a docker containier executing CNV calculation. Only supporting hg19
#' @description This function executes a ubuntu docker that produces as output oncosnp and request as input a tabl delimited file with Name	Chromosome	Position	Log R Ratio	B Allele Freq, where Name is the snp id from Illumina arrays, Chromosome is the chr, POsition is the SNP location in the chr Log R Ratio	B Allele Freq are genrated from genomestudio
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param scratch.folder, a character string indicating the scratch folder for temporary operations
#' @param sample.name, a character string indicating snp data file name
#' @param blood.name, a character string indicating snp data file name for blood snp
#'
#'
#' @return cnv1.cnvs, cnv2,cnvs and other oncosnp elements, see oncosnp help page for further information
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino
#'
#' @examples
#' \dontrun{
#'     system("wget http://130.192.119.59/public/testcnv.zip")
#'     #running fastqc
#'     oncosnp(group="docker", data.folder=getwd(), scratch.folder="/data/scratch", sample.name="sampleXX.txt", blood.name=NULL)
#' }
#'
#' @export
#'
oncosnp <- function(group=c("sudo","docker"), data.folder, scratch.folder, sample.name=NULL, blood.name=NULL){


  #storing the position of the home folder
  home <- getwd()



  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }

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


  #executing the docker job
  if(is.null(blood.name)){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data/ -v ", scratch.folder,":/scratch"," -d docker.io/repbioinfo/arraycnv.2019.01 bash /bin/oncosnp_command.sh ", sample.name, sep="")
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data/ -v ", scratch.folder,":/scratch"," -d docker.io/repbioinfo/arraycnv.2019.01 bash /bin/oncosnp_command2.sh ", sample.name, " ", blood.name, sep="")
  }
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nOncosnp analysis is finished\n")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("arraycnv user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("arraycnv system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("arraycnv elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("arraycnv user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("arraycnv system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("arraycnv elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/arraycnv_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
