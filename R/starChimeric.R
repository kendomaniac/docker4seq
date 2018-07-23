#' @title Running Star to detect chimeric transcripts on paired-end sequences
#' @description This function executes STAR to detect chimeric transcripts
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param threads, a number indicating the number of cores to be used from the application
#' @param chimSegmentMin, is a positive value indicating the minimal lenght of the overlap of a read to the chimeric element
#' @param chimJunctionOverhangMin, is a positive value indicating the minimum overhang for a chimeric junction
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running star2step nostrand pe
#'     starChimeric(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg38star", threads=8, chimSegmentMin=20, chimJunctionOverhangMin=15)
#'
#' }
#' @export
starChimeric <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, threads=1, chimSegmentMin=20, chimJunctionOverhangMin=15){

  home <- getwd()
  setwd(fastq.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }

  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(fastq.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scratch.folder, tmp.folder))
  dir.create(file.path(scratch.folder, tmp.folder,"/tmp"))
  dir <- dir(path=fastq.folder)
  dir.info <- dir[which(dir=="run.info")]
  if(length(dir.info)>0){
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",fastq.folder,"/run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))

  }
  #moving run.inf in scratch
  if(length(dir(fastq.folder)[grep("run.info",dir(fastq.folder))]) == 0){
    system(paste("touch ", scrat_tmp.folder,"/run.info",sep=""))
  }else{
    system(paste("mv run.info ", scrat_tmp.folder,sep=""))
  }

  dir <- dir[grep(".fastq.gz$", dir)]
  dir.trim <- dir[grep("trimmed", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", fastq.folder, "there are not fastq.gz files"))
    system("echo 1 > ExitStatusFile 2>&1")
    setwd(home)
    return(1)
  }else if(length(dir.trim)>0){
    dir <- dir.trim
    for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
    }
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("gzip -d ",scratch.folder,"/",tmp.folder,"/*.gz",sep=""))
  }else if(length(dir)>2){
    cat(paste("It seems that in ", fastq.folder, "there are more than two fastq.gz files"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }else{
    for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
    }
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("gzip -d ",scratch.folder,"/",tmp.folder,"/*.gz",sep=""))
  }

 params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",fastq.folder,":/fastq.folder -v ",scrat_tmp.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/star_chimeric_2.sh ",chimSegmentMin," ", chimJunctionOverhangMin," ", threads," ", sub(".gz$", "", dir[1])," ", sub(".gz$", "", dir[2]), sep="")
 resultRun <- runDocker(group=group, params=params)

 if(resultRun==0){
   #not saving fastq files
   cat("\nStar to detect chimeric transcripts is finished\n")
 }
    system(paste("mv ", scrat_tmp.folder,"/run.info ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Chimeric.out.sam ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Chimeric.out.junction ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Unmapped.out.mate1 ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Unmapped.out.mate2 ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Aligned.sortedByCoord.out.bam ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Log.out ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Log.final.out ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/Log.progress.out ",fastq.folder, sep=""))
    system(paste("mv ", scrat_tmp.folder,"/SJ.out.tab ",fastq.folder, sep=""))
    #running time 2
    ptm <- proc.time() - ptm
    con <- file(paste(fastq.folder,"run.info", sep="/"), "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,paste(fastq.folder,"run.info", sep="/"))
    #running time 2
    #removing temporary folder
    #saving log and removing docker container
    container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
#    system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
    system(paste("docker logs ", container.id, " >& ","starChimeric_",substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))


    cat("\n\nRemoving the rsemStar temporary file ....\n")
    system(paste("rm -R ",scrat_tmp.folder))
    system(paste("rm  -f ",fastq.folder,"/dockerID", sep=""))
    system(paste("rm  -f ",fastq.folder,"/tempFolderID", sep=""))
    setwd(home)

}
