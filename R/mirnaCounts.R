#' @title Counting miRNAs, Cordero et al. PLoS One. 2012;7:e31630
#' @description This function executes the docker container mrna8, which allows miRNAs counting.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where mirna fastq are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param mirbase.id, a character string indicating the mirbase prefix for the organism of interest, e.g. hsa for human or mmu for mouse
#' @param download.status, a boolean logical variable indicating if the latest mirbase database will be downloaded or the local mirbase 21 will be used. Default is FALSE
#' @param adapter.type, a character string. Two options: \code{"ILLUMINA"} or \code{"NEB"}, depending to which miRNA library prep was used: ILLUMINA or NEB
#' @param trimmed.fastq, a boolean logical variable indicating if trimmed fastq are saved. Default is FALSE
#' @author Raffaele Calogero
#'
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.
#' @examples
#'\dontrun{
#'    system("wget 130.192.119.59/public/test.mirnaCounts.zip")
#'    unzip("test.mirnaCounts.zip")
#'    setwd("test.mirnaCounts")
#'    library(docker4seq)
#'    mirnaCounts(group="docker",fastq.folder=getwd(),
#'              scratch.folder="/data/scratch",
#'              mirbase.id="hsa",download.status=FALSE,
#'              adapter.type="NEB", trimmed.fastq=FALSE)
#'
#' }
#' @export
mirnaCounts <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch",mirbase.id=c("hsa", "mmu"), download.status=FALSE, adapter.type=c("ILLUMINA","NEB"),  trimmed.fastq=FALSE){

  home <- getwd()

  #FastQC
  setwd(fastq.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  fastqc(group="docker", data.folder=getwd())

  #dir.fastq <- dir()
  #dir.fastq <- dir.fastq[grep("fastq.gz$", dir.fastq)]
  #system("mkdir fastQC.folder")
  #for(i in dir.fastq){
  #  system(paste("cp ", i, " fastQC.folder", sep=""))
  #  setwd("fastQC.folder")
  #  fastqc(group="docker", data.folder=getwd())
  #  j <- sub("fastq.gz","stdin_fastqc.html", i)
  #  system(paste("mv stdin_fastqc.html ", j, sep=""))
  #  system(paste("mv ", j, " ../", sep=""))
  #  z <- sub("fastq.gz","stdin_fastqc.zip", i)
  #  system(paste("mv stdin_fastqc.zip ", z, sep=""))
  #  system(paste("mv ", z, " ../", sep=""))
  #  system(paste("rm ", i, sep=""))
  #  setwd(fastq.folder)
  #}

  #setwd(fastq.folder)
  #system("mv ./fastQC.folder/*.log .")


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
	dir <- dir(path=fastq.folder)
	dir.info <- dir[which(dir=="run.info")]
	if(length(dir.info)>0){
	  system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	  system(paste("cp run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))
	}
	dir <- dir[grep(".fastq.gz$", dir)]
	if(length(dir)==0){
	  cat(paste("It seems that in ",fastq.folder, "there are not fastq.gz files"))
	  system("echo 1 > ExitStatusFile 2>&1")
	  setwd(home)
	  return(1)
	}else{
	  system(paste("chmod 777 -R", scrat_tmp.folder))
	  for(i in dir){
	    system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	  }
	  system(paste("chmod 777 -R",scrat_tmp.folder))
	}
	docker_fastq.folder=file.path("/data/scratch", tmp.folder)
	cat("\nsetting as working dir the scratch folder and running mirna8 docker container\n")

  params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/mirnaseq.2017.01 sh /bin/wrapperRun_local ", mirbase.id," ",docker_fastq.folder," ",download.status," ",adapter.type," ",trimmed.fastq, " ", fastq.folder, sep="")
  resultRun <- runDocker(group=group, params=params)

	if(resultRun==0){
	  cat("\nmirnaCounts analysis is finished\n")
	}

#	system(paste("chmod 777 -R", scrat_tmp.folder))
	con <- file(paste(scrat_tmp.folder,"out.info", sep="/"), "r")
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
	system(paste("rm ",scrat_tmp.folder,"/out.info",sep=""))

	#saving log and removing docker container
	container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& mirnaCounts_", substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))

	#removing temporary folder
	cat("\n\nRemoving the rsemStar temporary file ....\n")
#	system(paste("rm -R ",scrat_tmp.folder))
	system(paste("rm  ",fastq.folder,"/dockerID", sep=""))
	system(paste("rm  ",fastq.folder,"/tempFolderID", sep=""))
	#removing temporary folder
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
  all.counts <- read.table("all.counts.txt", sep="\t", header=T, row.names=1, stringsAsFactors = F, check.names = F)
	cpm <- apply(all.counts,2, function(x) (x/sum(x))*1000000)
	write.table(cpm, "cpm.txt", sep="\t")

	setwd(home)
}
