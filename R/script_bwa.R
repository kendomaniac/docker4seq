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
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     wget http://130.192.119.59/public/test_R1.fastq.gz
#'     wget http://130.192.119.59/public/test_R2.fastq.gz
#'     #running bwa
#'     bwa(group="sudo",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg19_exome", seq.type="pe", threads=24, sample.id="exome")
#' }

bwa <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", genome.folder, seq.type=c("se","pe"), threads=1, sample.id){
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
		system("sudo docker pull docker.io/rcaloger/bwa.1")
		if(seq.type=="pe"){
		      system(paste("sudo docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.sh ",file.path(scratch.folder, tmp.folder)," ",genome.folder," ", sample.id," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder, sep=""))
	    }else{
			  system(paste("sudo docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.sh ",file.path(scratch.folder, tmp.folder)," ",genome.folder," ", sample.id," ",fastq[1]," ", threads," ", fastq.folder,sep=""))
	    }
	}else{
		system("docker pull docker.io/rcaloger/bwa.1")
		if(seq.type=="pe"){
		      system(paste("docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.sh ",file.path(scratch.folder, tmp.folder)," ",genome.folder," ", sample.id," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder, sep=""))
	    }else{
			  system(paste("docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.sh ",file.path(scratch.folder, tmp.folder)," ",genome.folder," ", sample.id," ",fastq[1]," ", threads," ", fastq.folder,sep=""))
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
	con <- file(paste(file.path(scratch.folder, tmp.folder),"out.info", sep="/"), "r")
	tmp <- readLines(con)
	for(i in tmp){
		i <- sub("mv ",paste("mv ",file.path(scratch.folder, tmp.folder),"/",sep=""),i)
		system(i)
	}
}

