#' @title Running bwa, Li and Durbin Bioinformatics, 2009 Jul 15;25(14):1754-60
#' @description This function executes the docker container bwa1 where BWA is installed BWA is a read alignment package that efficiently align short sequencing reads against a large reference sequence This aligner provides optimal results with DNA-seq data
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa is located
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing
#' @param threads, a number indicating the number of cores to be used from the application
#' @param sample.id, a character string indicating the unique id to be associated to the bam that will be created
#' @param circRNA, a boolean variable indicating whether the analysis concerns a circRNA prediction or not.
#' @author Raffaele Calogero
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     #running bwa
#'     bwa(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg19bwa", seq.type="se",
#'     threads=24, sample.id="exome")
#'
#' }
#' @export
bwa <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, seq.type=c("se","pe"), threads=1, sample.id, circRNA=FALSE){


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
      system("echo 10 >& ExitStatusFile")
      setwd(home)
      return(10)
    }
    #########check scratch folder exist###########
    if (!file.exists(scratch.folder)){
      cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
      system("echo 3 >& ExitStatusFile")
      setwd(home)
      return(3)
    }
    #############################################
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
    dir <- dir[grep(".fastq.gz$", dir)]
    dir.trim <- dir[grep("trimmed", dir)]
    cat("\ncopying \n")
    if(length(dir)==0){
      cat(paste("It seems that in ", fastq.folder, "there are not fastq.gz files"))
      system("echo 1 >& ExitStatusFile")
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
      system("echo 2 >& ExitStatusFile")
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
    docker_fastq.folder=scrat_tmp.folder
    #Trimmed fastq  linking fpr docker
    fastq <- sub(".gz$", "", dir)
    cat("\nsetting as working dir the scratch folder and running  docker container\n")
    cat("\nsetting as working dir the scratch folder and running bwa docker container\n")

if(circRNA == FALSE){
    if(seq.type=="pe"){
		      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",docker_fastq.folder,":/data/scratch -v ",genome.folder,":/data/genome -v ", fastq.folder,":/fastq.folder -d docker.io/repbioinfo/bwa.2019.01 sh /bin/bwa_pe.sh /data/scratch ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", sample.id, " ",fastq.folder, sep="")
		      resultRun <- runDocker(group=group, params=params)
	  }else if(seq.type=="se"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",docker_fastq.folder,":/data/scratch -v ",genome.folder,":/data/genome -v ", fastq.folder,":/fastq.folder -d docker.io/repbioinfo/bwa.2019.01 sh /bin/bwa_se.sh /data/scratch ", threads," ", fastq[1]," /data/genome ", sample.id, " ",fastq.folder, sep="")
		    resultRun <- runDocker(group=group, params=params)
	  }
}

if(circRNA == TRUE){
    if(seq.type=="pe"){
		      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",docker_fastq.folder,":/data/scratch -v ",genome.folder,":/data/genome -v ", fastq.folder,":/fastq.folder -d docker.io/repbioinfo/bwa.2019.01 sh /bin/bwa_pe_ciri.sh /data/scratch ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", sample.id, " ",fastq.folder, sep="")
		      resultRun <- runDocker(group=group, params=params)
	  }else if(seq.type=="se"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",docker_fastq.folder,":/data/scratch -v ",genome.folder,":/data/genome -v ", fastq.folder,":/fastq.folder -d docker.io/repbioinfo/bwa.2019.01 sh /bin/bwa_se_ciri.sh /data/scratch ", threads," ", fastq[1]," /data/genome ", sample.id, " ",fastq.folder, sep="")
		    resultRun <- runDocker(group=group, params=params)
		  }
}


    if(resultRun==0){
      system(paste("cp ", docker_fastq.folder, "/* ", fastq.folder, sep=""))
      cat("\nBWA analysis is finished\n")    
    }
    #running time 2
    ptm <- proc.time() - ptm
    dir <- dir(fastq.folder)
    dir <- dir[grep("run.info",dir)]
    if(length(dir)>0){
      con <- file("run.info", "r")
      tmp.run <- readLines(con)
      close(con)
      tmp.run[length(tmp.run)+1] <- paste("BWA user run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("BWA system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("BWA elapsed run time mins ",ptm[3]/60, sep="")
      writeLines(tmp.run,"run.info")
    }else{
      tmp.run <- NULL
      tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("BWA system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("BWA elapsed run time mins ",ptm[3]/60, sep="")

      writeLines(tmp.run,"run.info")
    }

    #saving log and removing docker container
    container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
    system(paste("docker logs ", container.id, " >& ", "bwa_",substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))

    #removing temporary folder
    cat("\n\nRemoving the bwa temporary file ....\n")
    system(paste("rm -R ",scrat_tmp.folder))
    system(paste("rm  -f ",fastq.folder,"/dockerID", sep=""))
    system(paste("rm  -f ",fastq.folder,"/tempFolderID", sep=""))

    system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
    setwd(home)
}
