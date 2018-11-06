#' @title Create a bigWig from the output of bowtie2
#' @description This function executes the docker container bowtie2 embedding deeptools the output of bowtie2 sorted.bam is used to generate the bigWig
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where bam, bai files  are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param threads, a number indicating the number of cores to be used from the application
#' @author Raffaele Calogero
#'
#' @return sorted.bw
#' @examples
#'\dontrun{
#'     #downloading fastq files
#' system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#' system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#' library(docker4seq)
#' deeptoolBwig(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch/", threads=8)
#'
#' }
#' @export
deeptoolBwig <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", threads){

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
	dir <- dir(path=fastq.folder)
	dir <- dir[grep(".bam", dir)]
	cat("\ncopying \n")
	if(length(dir)==0){
		cat(paste("It seems that in ", fastq.folder, "there is not a bam file"))
	  system("echo 1 > ExitStatusFile 2>&1")
		return(1)
	}else{
	  bam=dir[1]
	  bai=dir[2]
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		for(i in dir){
		      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	    }
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}
	docker_fastq.folder=file.path("/data/scratch", tmp.folder)
	cat("\nsetting as working dir the scratch folder and running  docker container\n")

		    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch -d repbioinfo/bowtie2.2018.01:bowtie2-2.2.9-R3.4.4-Bioconductor3.6-Biostrings_2.46.0 sh /bin/deeptoolsBwig.sh ",docker_fastq.folder," ", bam," ",threads," ", fastq.folder, sep="")
		    resultRun <- runDocker(group=group, params=params)

	if(resultRun==0){
	  cat("\nbigWig creation is finished\n")
	  system(paste("cp ",scrat_tmp.folder,"/*.bw ", fastq.folder, sep=""))
	  system(paste("cp ",scrat_tmp.folder,"/run.info ", fastq.folder, sep=""))
	}

	#running time 2
	ptm <- proc.time() - ptm
	run.info.file <- grep("run.info", dir(fastq.folder))
	tmp.run[length(tmp.run)+1] <- paste("bigWig user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("bigWig system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("bigWig elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run,paste(fastq.folder,"run.info", sep="/"))
	#saving log and removing docker container
	container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& ", "bowtie_",substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))


	#removing temporary folder
	cat("\n\nRemoving the bigWig temporary file ....\n")
#	system(paste("rm -R ",scrat_tmp.folder))
	system(paste("rm  ",fastq.folder,"/dockerID", sep=""))
	system(paste("rm  ",fastq.folder,"/tempFolderID", sep=""))
	#removing temporary folder
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))

	setwd(home)

}

