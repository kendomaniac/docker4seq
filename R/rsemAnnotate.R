#' @title Annotating RSEM gene.results using ENSEMBL annotation
#' @description This function executes the docker container annotate.1, where Bioconductor is used to annotated gene.results output of RSEM using ENSEMBL annotation
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param rsem.folder, a character string indicating where gene.results and isoforms.results are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param org, a character string indicating the genome assembly used for mapping and counting with \code{"rsemstar"} function
#' @param truncating.expected.counts, a boolean logical variable indicating if the expected counts calculated by RSEM need to be converted in integer to be compliant with differnetial expression Bioconductor packages as DESeq2. Default is FALSE
#' @param protein.anno, a boolean logical variable indicating if instead of gene SYMBOL SWISSPROT symbol are used. This option is useful for integrating transcriptomics data with proteomics data
#'
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/genes.results.gz")
#'     gzip -d genes.results.gz
#'     #running rsemanno
#'     rsemanno(group="docker",rsem.folder=getwd(), scratch.folder="/data/scratch",
#'     org="hg38", truncating.expected.counts=FALSE,
#'     protein.anno=FALSE)
#'
#' }
#' @export
rsemanno <- function(group=c("sudo","docker"),rsem.folder=getwd(), scratch.folder="/data/scratch",org=c("hg19", "hg38", "mm10","mm9"), truncating.expected.counts=FALSE, protein.anno=FALSE){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #########check scratch folder exist###########
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    return(3)
  }
  #############################################
  annotation.type="rsemENSEMBL"
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
	cat("\ncreating a folder in scratch folder\n")
    dir.create(file.path(scratch.folder, tmp.folder))
	dir <- dir(path=rsem.folder)
	dir.info <- dir[which(dir=="run.info")]
	if(length(dir.info)>0){
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	  system(paste("cp ",rsem.folder,"/run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))
	}
	dir <- dir[grep("genes.results", dir)]
	cat("\ncopying \n")
	if(length(dir)==0){
		cat(paste("It seems that in ", rsem.folder, "there is not a genes.results file generated using rsemstar"))
		return(1)
	}else if(length(dir)>1){
		cat(paste("It seems that in ", rsem.folder, "there are more than one genes.results files"))
		return(2)
	}else{
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		system(paste("cp ",getwd(),"/",dir, " ",scratch.folder,"/",tmp.folder,"/",dir, sep=""))
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}
	cat("\nsetting as working dir the scratch folder and running annotate docker container\n")
  rsem.results <- dir
	if(group=="sudo"){
#		      system("sudo docker pull docker.io/rcaloger/annotate.2017.01")
		      system(paste("sudo docker run --privileged=true -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/annotate.2017.01 sh /bin/annotate.sh ", file.path(scratch.folder, tmp.folder)," ",rsem.results," ",org," ",truncating.expected.counts," ",annotation.type," ", protein.anno, " ", rsem.folder, sep=""))
	}else{
#	        system("docker pull docker.io/rcaloger/annotate.2017.01")
	        system(paste("docker run --privileged=true  -v ",scratch.folder,":/data/scratch"," -d docker.io/rcaloger/annotate.2017.01 sh /bin/annotate.sh ", file.path(scratch.folder, tmp.folder)," ",rsem.results," ",org," ",truncating.expected.counts," ",annotation.type," ", protein.anno, " ", rsem.folder, sep=""))

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
	con <- file(paste(rsem.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)
	tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run,paste(rsem.folder,"run.info", sep="/"))
	#running time 2
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",rsem.folder, sep=""))
}

