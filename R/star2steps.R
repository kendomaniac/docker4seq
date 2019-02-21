#' @title Running Star two steps for variant calls
#' @description This function executes the two steps STAR as sugested by best practice GATK for calling variants on RNAseq data only PE data are accepted
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param groupid, a character string to be inserted in the bam as identifier for the sample
#' @param threads, a number indicating the number of cores to be used from the application
#' @param opossum.preprocessing, a boolean TRUE or FALSE to use opossum for RNAseq data preprocessing https://wellcomeopenresearch.org/articles/2-6/v1
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running star2step nostrand pe
#'     star2steps(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg38star", groupid="test", threads=8, opossum.preprocessing=FALSE)
#'
#' }
#' @export
star2steps <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, groupid, threads=1, opossum.preprocessing=FALSE){

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
    system(paste("cp ",scratch.folder,"/run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))

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
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
    }
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }else if(length(dir)>2){
    cat(paste("It seems that in ", fastq.folder, "there are more than two fastq.gz files"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }else{
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
    }
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }
  #Trimmed fastq  linking fpr docker
  docker_fastq.folder=file.path("/data/scratch", tmp.folder)
  #Trimmed fastq  linking fpr docker
  fastq <- sub(".gz$", "", dir)
  cat("\nsetting as working dir the scratch folder and running  docker container\n")


  if(group=="sudo"){
  #    system("sudo docker pull docker.io/repbioinfo/star251.2017.01")
      if(opossum.preprocessing){
        params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/2step_star_opossum.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep="")
        resultRun <- runDocker(group="sudo", params=params)

#        system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/star25.1 sh /bin/2step_star_opossum.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep=""))
      }else{
        params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/2step_star.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep="")
        resultRun <- runDocker(group="docker", params=params)

#        system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/star25.1 sh /bin/2step_star.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep=""))
      }
   }else{
#        system("docker pull docker.io/repbioinfo/star251.2017.01")
        if(opossum.preprocessing){
            params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",docker_fastq.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/2step_star_opossum.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep="")
            resultRun <- runDocker(group="sudo", params=params)

#          system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/star25.1 sh /bin/2step_star_opossum.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep=""))
        }else{
           params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/2step_star.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep="")
           resultRun <- runDocker(group="docker", params=params)

#          system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/star25.1 sh /bin/2step_star.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", groupid, " ", fastq.folder, sep=""))
        }
   }
  if(resultRun==0){
    out <- "out.info"
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
    system(paste("docker logs ", container.id, " >& ","star2steps_",substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))


    cat("\n\nRemoving the rsemStar temporary file ....\n")
    system(paste("rm -R ",scrat_tmp.folder))
    system(paste("rm  -f ",fastq.folder,"/dockerID", sep=""))
    system(paste("rm  -f ",fastq.folder,"/tempFolderID", sep=""))


  }
  setwd(home)
}
