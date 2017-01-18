#' @title Running RSEM, Li and Dewey BMC Bioinformatics 2011 12:323
#' @description This function executes the docker container rsemstar1, where RSEM is used to calculate gene/isoforms counts using as mapper STAR, Dubin et al. Bioinformatics. 2013 Jan 1;29(1):15-21
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa is located. IMPORTANT the present function only suport genomic indexes made using ensembl genom and the corresponding gtf
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing
#' @param strandness, a character string indicating the type ofsequencing protocol used for the analysis. Three options: \code{"none"}, \code{"forward"}, \code{"reverse"} respectively for non strand selection, forward for Illumina strandness protocols, reverse for ACCESS Illumina protocol
#' @param threads, a number indicating the number of cores to be used from the application
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running rsemstar nostrand pe
#'     rsemstar(group="sudo",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg38star", seq.type="pe", strandness="none",
#'     threads=24)
#'
#' }
#' @export
rsemstar <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, seq.type=c("se","pe"), strandness=c("none","forward","reverse"), threads=1){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
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
	dir <- dir[grep(".fastq.gz", dir)]
	dir.trim <- dir[grep("trimmed", dir)]
	cat("\ncopying \n")
	if(length(dir)==0){
		cat(paste("It seems that in ", getwd(), "there are not fastq.gz files"))
		return(1)
	}else if(length(dir.trim)>0){
	  dir <- dir.trim
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	  for(i in dir){
	    system(paste("cp ",getwd(),"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	  }
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}else if(length(dir)>2){
		cat(paste("It seems that in ", getwd(), "there are more than two fastq.gz files"))
		return(2)
	}else{
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		for(i in dir){
		      system(paste("cp ",getwd(),"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	    }
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}
	fastq <- sub(".gz$", "", dir)
	cat("\nsetting as working dir the scratch folder and running bwa docker container\n")

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/rsemstar.2017.01")
		if(seq.type=="pe"){
		  if(strandness=="none"){
		      system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_nostrand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
		  }else if(strandness=="forward"){
		    system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_strand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
		  }else if(strandness=="reverse"){
		    system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_access.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
		  }

		}else{
		  if(strandness=="none"){
	        system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_nostrand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
		  }else if(strandness=="forward"){
		    system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_strand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
		  }else if(strandness=="reverse"){
		    system(paste("sudo docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_access.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
		  }
		}
	}else{
	  if(seq.type=="pe"){
	    if(strandness=="none"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_nostrand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
	    }else if(strandness=="forward"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_strand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
	    }else if(strandness=="reverse"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem2_access.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", fastq[2]," ", genome.folder," ", fastq.folder, sep=""))
	    }

	  }else{
	    if(strandness=="none"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_nostrand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
	    }else if(strandness=="forward"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_strand.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
	    }else if(strandness=="reverse"){
	      system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar.2017.01 sh /bin/rsem1_access.sh ",file.path(scratch.folder, tmp.folder)," ", threads," ", fastq[1]," ", genome.folder," ", fastq.folder, sep=""))
	    }
	  }

	}
	star <- "xxxx"
	while(star != "Log.final.out"){
	  Sys.sleep(10)
	  cat("-")
	  #checking the stas for STAR
	  star.tmp <- dir(paste(file.path(scratch.folder, tmp.folder),"/xxx.temp/", sep=""))
	  star.tmp <- star.tmp[grep("Log.final.out",star.tmp)]
	  if(length(star.tmp)>0){
	    star <- "Log.final.out"
	    system(paste("cp ",file.path(scratch.folder, tmp.folder),"/xxx.temp/xxxLog.final.out ", file.path(scratch.folder, tmp.folder),"/Log.final.out",sep=""))
	  }
	}
	out <- "xxxx"
	#waiting for the end of the container work
  while(out != "out.info"){
		Sys.sleep(10)
		cat(".")
		out.tmp <- dir(file.path(scratch.folder, tmp.folder))
		out.tmp <- out.tmp[grep("out.info",out.tmp)]

		if(length(out.tmp)>0){
			out <- "out.info"
		}
  }
	system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
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

}

