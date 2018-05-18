#' @title Running realignment and recalibration, GATK
#' @description This function executes the docker container snv.1 where GATK software is used to do INDEL realignment and quality recalibration. This analysis is required only to run mutect1. The bwa index has to be prepared with bwaIndex
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param bam.folder, a character string indicating where bam files generated with bwa.R are located. In this folder should be loacted also the GATK file GenomeAnalysisTK-X.X-0.tar.bz2.
#' @param gatk.filename, a character string for GenomeAnalysisTK-X.X-0.tar.bz2.
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa is located
#' @param threads, a number indicating the number of cores to be used from the application
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running bwa
#'     gatkDNA(group="sudo",bam.folder=getwd(), scratch.folder="/data/scratch",
#'     gatk.filename="GenomeAnalysisTK-3.7.tar.bz2"
#'     genome.folder="/data/scratch/hg19_bwa", threads=24)
#' }
#' @export
gatkDNA <- function(group=c("sudo","docker"), bam.folder=getwd(), scratch.folder="/data/scratch", gatk.filename, genome.folder, threads=1){
  #running time 1
  ptm <- proc.time()
  
  #remembering actual folder
  home <- getwd()
  #setting rsem output folder as working dir
  setwd(bam.folder)
  
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }
  
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scratch.folder, tmp.folder))
  dir.create(file.path(scratch.folder, tmp.folder,"/tmp"))
  dir <- dir()
  dir.info <- dir[which(dir=="run.info")]
  if(length(dir.info)>0){
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))
  }
  dir <- dir[grep("dedup_reads.bam", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", getwd(), "there is not dedup_reads.bam"))
    system("echo 1 > ExitStatusFile 2>&1")
    setwd(home)
    return(1)
  }
  docker_bam.folder=file.path("/data/scratch", tmp.folder)
  system(paste("chmod 777 -R", docker_bam.folder))
  system(paste("cp ",getwd(),"/dedup_reads.bam ",docker_bam.folder,"/dedup_reads.bam", sep=""))
  system(paste("cp ",getwd(),"/dedup_reads.bai ",docker_bam.folder,"/dedup_reads.bai", sep=""))
  system(paste("cp ",gatk.filename, " ",docker_bam.folder,"/GenomeAnalysisTK.tar.bz2", sep=""))
  if(group=="sudo"){
    system("sudo docker pull docker.io/rcaloger/snv.1")
    system(paste("sudo docker run --privileged=true --cidfile ",bam.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/snv.1 sh /bin/gatk.sh ",docker_bam.folder," ", threads," ", bam.folder, sep=""))
  }else{
    system("docker pull docker.io/rcaloger/snv.1")
    system(paste("docker run --privileged=true --cidfile ",bam.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/snv.1 sh /bin/gatk.sh ",docker_bam.folder," ", threads," ", bam.folder, sep=""))
  }
  out <- "xxxx"
  #waiting for the end of the container work
  while(out != "out.info"){
    Sys.sleep(10)
    cat(".")
    out.tmp <- dir(docker_bam.folder)
    out.tmp <- out.tmp[grep("out.info",out.tmp)]

    if(length(out.tmp)>0){
      out <- "out.info"
    }
  }
  con <- file(paste(docker_bam.folder,"out.info", sep="/"), "r")
  tmp <- readLines(con)
  close(con)
  for(i in tmp){
    i <- sub("mv ",paste("mv ",docker_bam.folder,"/",sep=""),i)
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
  #running time 2
  #removing temporary folder
  cat("\n\nRemoving the rsemStar temporary file ....\n")
#  system(paste("rm -R ",docker_bam.folder))
  system(paste("rm  ",bam.folder,"/dockerID", sep=""))
  #removing temporary folder
  setwd(home)
}
