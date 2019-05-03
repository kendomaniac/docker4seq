#' @title Running RSEM, Li and Dewey BMC Bioinformatics 2011 12:323
#' @description This function executes the docker container rsemstar1, where RSEM is used to calculate gene/isoforms counts using as mapper STAR, Dubin et al. Bioinformatics. 2013 Jan 1;29(1):15-21
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located. IMPORTANT the present function only suport genomic indexes made using ensembl genom and the corresponding gtf
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing
#' @param strandness, a character string indicating the type ofsequencing protocol used for the analysis. Three options: \code{"none"}, \code{"forward"}, \code{"reverse"} respectively for non strand selection, forward for Illumina strandness protocols, reverse for ACCESS Illumina protocol
#' @param threads, a number indicating the number of cores to be used from the application
#' @param save.bam, a boolean TRUE FALSE to decide if bam files are saved
#' @author Raffaele Calogero
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#' system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#' system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#' library(docker4seq)
#' #running rsemstar nostrand pe
#' rsemstar(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch/",
#'          genome.folder="/data/genomes/hg38star/", seq.type="pe", strandness="none",
#'          threads=8, save.bam = FALSE)
#'
#' }
#' @export
rsemstar <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, seq.type=c("se","pe"), strandness=c("none","forward","reverse"), threads=1, save.bam = TRUE){

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
	  system("echo 1 > ExitStatusFile 2>&1")
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
#		system("sudo docker pull docker.io/repbioinfo/rsemstar.2017.01")
		if(seq.type=="pe"){
		  if(strandness=="none"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_nostrand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }else if(strandness=="forward"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_strand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }else if(strandness=="reverse"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_access.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }
		}else{
		  if(strandness=="none"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_nostrand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }else if(strandness=="forward"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_strand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }else if(strandness=="reverse"){
		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_access.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
		    resultRun <- runDocker(group="sudo", params=params)
		  }
		}
	}else{
	  if(seq.type=="pe"){
	    if(strandness=="none"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_nostrand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }else if(strandness=="forward"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_strand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }else if(strandness=="reverse"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem2_access.sh ",docker_fastq.folder," ", threads," ", fastq[1]," ", fastq[2]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }

	  }else{
	    if(strandness=="none"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_nostrand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }else if(strandness=="forward"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_strand.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }else if(strandness=="reverse"){
	      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsem1_access.sh ",docker_fastq.folder," ", threads," ", fastq[1]," /data/genome ", fastq.folder, sep="")
	      resultRun <- runDocker(group="docker", params=params)
	    }
	  }

	}

	if(resultRun==0){
	  cat("\nThe RSEM STAR analysis is finished\n")
	}

	#star logs
	star.tmp <- dir(paste(file.path(scratch.folder, tmp.folder),"/xxx.temp/", sep=""))
	star.tmp <- star.tmp[grep("Log.final.out",star.tmp)]
		  if(length(star.tmp)>0){
		    star <- "Log.final.out"
		    system(paste("cp ",file.path(scratch.folder, tmp.folder),"/xxx.temp/xxxLog.final.out ", file.path(scratch.folder, tmp.folder),"/Log.final.out",sep=""))
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
	con <- file(paste(fastq.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)
	tmp.run[length(tmp.run)+1] <- paste("RSEM user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("RSEM system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("RSEM elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run,paste(fastq.folder,"run.info", sep="/"))
	#running time 2
	if (save.bam) {
	  system(paste("cp ", file.path(scratch.folder, tmp.folder),
	               "/xxx.temp/xxx.bam ", file.path(fastq.folder), "/Aligned.out.bam",
	               sep = ""))
	  system(paste("cp ", file.path(scratch.folder, tmp.folder),
	               "/xxx.transcript.bam ", file.path(fastq.folder),
	               "/Aligned.toTranscriptome.out.bam", sep = ""))
	}

	#saving log and removing docker container
	container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& ", "rsemstar_",substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))


	#removing temporary folder
	cat("\n\nRemoving the rsemStar temporary file ....\n")
	system(paste("rm -R ",scrat_tmp.folder))
	system(paste("rm  ",fastq.folder,"/dockerID", sep=""))
	system(paste("rm  ",fastq.folder,"/tempFolderID", sep=""))
	#removing temporary folder
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))

	setwd(home)

}
