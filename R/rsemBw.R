#' @title Creating a bigwig using RSEM
#' @description This function executes the docker container rsemstar where RSEM is installed and create a bigwig file for genomic data visualization
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param bam.folder, a character string indicating where BAM SORTED file is located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#'
#' @return output.bw, which is the bigwig
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     rsemBw(group="docker",bam.folder=getwd(), scratch.folder="/data/scratch")
#'

#'
#' }
#' @export
rsemBw <- function(group=c("sudo","docker"),bam.folder=getwd(), scratch.folder="/data/scratch"){
  #running time 1
  ptm <- proc.time()
  #running time 1
  
  home <- getwd()
  setwd(bam.folder)
  
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }
  #########check scratch folder exist###########
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }
  #############################################
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(bam.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scratch.folder, tmp.folder))
  dir.create(file.path(scratch.folder, tmp.folder,"/tmp"))
  dir <- dir(path=bam.folder)
  dir.info <- dir[which(dir=="run.info")]
  if(length(dir.info)>0){
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",bam.folder,"/run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))
    
  }
  dir <- dir[grep(".bam", dir)]
  dir <- dir[which(dir=="Aligned.out.bam")]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", bam.folder, "there is not a bam file"))
    return(1)
  }else{
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",bam.folder,"/",dir, " ",scratch.folder,"/",tmp.folder,"/",dir, sep=""))
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }
  docker_fastq.folder=file.path("/data/scratch", tmp.folder)

  params <- paste("--cidfile ",bam.folder,"/dockerID -v ",scratch.folder,":/data/scratch -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem_bw.sh ",docker_fastq.folder," ", dir," ",bam.folder, sep="")
  resultRun=runDocker(group="sudo", params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    cat("\n bigwig using RSEM  is finished\n")
  }
  
  #system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  con <- file(paste(file.path(scratch.folder, tmp.folder),"out.info", sep="/"), "r")
  tmp <- readLines(con)
  close(con)
  for(i in tmp){
    i <- sub("mv ",paste("mv ",file.path(scratch.folder, tmp.folder),"/",sep=""),i)
    system(i)
  }
  #running time 2
  ptm <- proc.time() - ptm
  con <- file(paste(bam.folder,"run.info", sep="/"), "r")
  tmp.run <- readLines(con)
  close(con)
  tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
  writeLines(tmp.run,paste(bam.folder,"run.info", sep="/"))
  
  #saving log and removing docker container
  container.id <- readLines(paste(bam.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  
  #removing temporary folder
  cat("\n\nRemoving the rsem temporary file ....\n")
 #  system(paste("rm -R ",scrat_tmp.folder))
 #  system(paste("rm  -f ",bam.folder,"/dockerID", sep=""))
 #  system(paste("rm  -f ",bam.folder,"/tempFolderID", sep=""))
  
  setwd(home)
}

