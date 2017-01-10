#' @title Counting miRNAs, Cordero et al. PLoS One. 2012;7(2):e31630
#' @description This function executes the docker container mrna8, which allows miRNAs counting.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where mirna fastq are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param mirbase.id, a character string indicating the mirbase prefix for the organism of interest, e.g. hsa for human or mmu for mouse
#' @param download.status, a boolean logical variable indicating if the latest mirbase database will be downloaded or the local mirbase 21 will be used. Default is FALSE
#' @param adapter.type, a character string. Two options: \code{"ILLUMINA"} or \code{"NEB"}, depending to which miRNA library prep was used: ILLUMINA or NEB
#' @param trimmed.fastq, a boolean logical variable indicating if trimmed fastq are saved. Default is FALSE
#'
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     wget http://130.192.119.59/public/ctrl1_R1.fastq.gz
#'     wget http://130.192.119.59/public/ctrl2_R1.fastq.gz
#'     wget http://130.192.119.59/public/ctrl3_R1.fastq.gz
#'     wget http://130.192.119.59/public/trt1_R1.fastq.gz
#'     wget http://130.192.119.59/public/trt2_R1.fastq.gz
#'     #running mirnaCounts
#'     mirnaCounts(group="sudo",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     mirbase.id="hsa",download.status=FALSE, adapter.type="NEB", trimmed.fastq=FALSE)
#'
#' }
#' @export
mirnaCounts <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch",mirbase.id=c("hsa", "mmu"), download.status=FALSE, adapter.type=c("ILLUMINA","NEB"),  trimmed.fastq=FALSE){
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
	dir <- dir()
	dir.info <- dir[which(dir=="run.info")]
	if(length(dir.info)>0){
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	  system(paste("cp run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))
	}
	dir <- dir[grep(".fastq.gz", dir)]
	if(length(dir)==0){
	  cat(paste("It seems that in ", getwd(), "there are not fastq.gz files"))
	  return(1)
	}else{
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	  for(i in dir){
	    system(paste("cp ",getwd(),"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	  }
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}

	cat("\nsetting as working dir the scratch folder and running mirna8 docker container\n")
	if(group=="sudo"){
		      system("sudo docker pull docker.io/rcaloger/mirna8")
		      system(paste("sudo docker run -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/mirna8 sh /bin/wrapperRun_local ", mirbase.id," ",file.path(scratch.folder, tmp.folder)," ",download.status," ",adapter.type," ",trimmed.fastq, " ", fastq.folder, sep=""))
	}else{
	        system("docker pull docker.io/rcaloger/mirna8")
	        system(paste("docker run -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/mirna8 sh /bin/wrapperRun_local ", mirbase.id," ",file.path(scratch.folder, tmp.folder)," ",download.status," ",adapter.type," ",trimmed.fastq, " ", fastq.folder, sep=""))

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
	tmp.run <- NULL
	tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run,paste(fastq.folder,"run.info", sep="/"))
	#running time 2

}

