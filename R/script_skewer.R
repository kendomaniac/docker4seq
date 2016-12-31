#' @title Running skewer, an adapter trimmer application, Jiang et al BMC Bioinformatics201415:182 
#' @description This function executes the docker container skewer1 to remove sequencing adapters from RNAseq reads.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs. 
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param adapter5, a character string indicating the fwd adapter
#'@param adapter3, a character string indicating the rev adapter
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing. 
#'@param threads, a number indicating the number of cores to be used from the application.
#'@param min.length, a number indicating minimal length required to return a trimmed read.
#' @return One or two gzip fastq file ending with trimmed-pair1.fastq.gz and trimmed-pair1.fastq.gz, a log file of the trimming with the extensione trimmed.log, run.info file descring the analysis steps done by the docker. The latter file is useful to understand where the docker stop in case of unexpected end. 
#' @examples
#'\dontrun{
#'     wget http://130.192.119.59/public/test_R1.fastq.gz
#'     wget http://130.192.119.59/public/test_R2.fastq.gz
#'     skewer(group="sudo",fastq.folder=getwd(), scratch.folder="/data/scratch", 
#'     adapter5="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT", 
#'     adapter3="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT", 
#'     seq.type="pe", threads=10,  min.length=40)
#' }

skewer <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", adapter5, adapter3, seq.type=c("se","pe"), threads=1, min.length=18){
	tmp.folder <- gsub(":","-",gsub(" ","-",date()))
	cat("\ncreating a folder in scratch folder\n")
    dir.create(file.path(scratch.folder, tmp.folder))
	dir <- dir()
	dir <- dir[grep(".fastq.gz", dir)]
	cat("\ncopying and unzipping\n")
	if(length(dir)==0){
		cat(paste("It seems that in ", getwd(), "there are not fastq.gz files"))
		return(1)
	}else if(length(dir)>2){
		cat(paste("It seems that in ", getwd(), "there are more than two fastq.gz files"))
		return(2)
	}else{
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		for(i in dir){
		      system(paste("cp ",getwd(),"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	#		  untar(paste(scratch.folder,tmp.folder,i,sep="/"), compressed = 'gzip')
	    }
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		system(paste("gzip -d ",scratch.folder,"/",tmp.folder,"/*.gz",sep=""))
	}
	fastq <- sub(".gz$", "", dir)
	cat("\nsetting as working dir the scratch folder and running skewer docker container\n")

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/skewer1")
		if(seq.type=="pe"){
		      system(paste("sudo docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/skewer1 sh /bin/trim2.sh ",file.path(scratch.folder, tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder," ", min.length, sep=""))
	    }else{
			  system(paste("sudo docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/skewer1 sh /bin/trim1.sh ",file.path(scratch.folder, tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", threads," ", fastq.folder," ", min.length, sep=""))
	    }
	}else{
		system("docker pull docker.io/rcaloger/skewer1")
		if(seq.type=="pe"){
		      system(paste("docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/skewer1 sh /bin/trim2.sh ",file.path(scratch.folder, tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder," ", min.length, sep=""))
	    }else{
			  system(paste("docker run -v ",scratch.folder,":", scratch.folder," -d docker.io/rcaloger/skewer1 sh /bin/trim1.sh ",file.path(scratch.folder, tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", threads," ", fastq.folder," ", min.length , sep=""))
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

