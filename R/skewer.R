#' @title Running skewer, an adapter trimmer application, Jiang et al BMC Bioinformatics201415:182
#' @description This function executes the docker container skewer1 to remove sequencing adapters from RNAseq reads
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param adapter5, a character string indicating the fwd adapter
#' @param adapter3, a character string indicating the rev adapter
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing.
#' @param threads, a number indicating the number of cores to be used from the application
#' @param min.length, a number indicating minimal length required to return a trimmed read
#' @author Raffaele Calogero
#'
#' @return One or two gzip fastq files ending with trimmed-pair1.fastq.gz and trimmed-pair1.fastq.gz, a log file of the trimming with the extensione trimmed.log, run.info file descring the analysis steps done by the docker. The latter file is useful to understand where the docker stop in case of unexpected end
#' @examples
#'\dontrun{
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     skewer(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     adapter5="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#'     adapter3="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
#'     seq.type="pe", threads=10,  min.length=40)
#' }
#' @export
skewer <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder="/data/scratch", adapter5, adapter3, seq.type=c("se","pe"), threads=1, min.length=18){

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
	cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scratch.folder, tmp.folder))
  writeLines(scrat_tmp.folder,paste(fastq.folder,"/tempFolderID", sep=""))
	dir <- dir(path=fastq.folder)
	dir <- dir[grep(".fastq.gz$", dir)]
	cat("\ncopying and unzipping\n")
	if(length(dir)==0){
		cat(paste("It seems that in ",fastq.folder, "there are not fastq.gz files"))

	  system("echo 1 > ExitStatusFile 2>&1")
		setwd(home)
		return(1)
	}else if(length(dir)>2){
		cat(paste("It seems that in ",fastq.folder, "there are more than two fastq.gz files"))
	  system("echo 2 > ExitStatusFile 2>&1")
		setwd(home)
		return(2)
	}else{
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		for(i in dir){
		      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	#		  untar(paste(scratch.folder,tmp.folder,i,sep="/"), compressed = 'gzip')
	    }
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		system(paste("gzip -d ",scratch.folder,"/",tmp.folder,"/*.gz",sep=""))
	}
	fastq <- sub(".gz$", "", dir)
	cat("\nsetting as working dir the scratch folder and running skewer docker container\n")

	if(group=="sudo"){
#		system("sudo docker pull docker.io/repbioinfo/skewer.2017.01")
		if(seq.type=="pe"){
		      params <- paste("--cidfile ",fastq.folder,"/dockerID   -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/skewer.2017.01 sh /bin/trim2.sh ",file.path("/data/scratch", tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder," ", min.length, sep="")
		      resultRun <- runDocker(group="sudo", params=params)
		}else{
			    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/skewer.2017.01 sh /bin/trim1.sh ",file.path("/data/scratch", tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", threads," ", fastq.folder," ", min.length, sep="")
			    resultRun <- runDocker(group="sudo", params=params)
		}
	}else{
#		system("docker pull docker.io/repbioinfo/skewer.2017.01")
		if(seq.type=="pe"){
		      params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/skewer.2017.01 sh /bin/trim2.sh ",file.path("/data/scratch", tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", fastq[2]," ", threads," ", fastq.folder," ", min.length, sep="")
		      resultRun <- runDocker(group="docker", params=params)
		}else{
			   params <- paste("--cidfile ",fastq.folder,"/dockerID -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/skewer.2017.01 sh /bin/trim1.sh ",file.path("/data/scratch", tmp.folder)," ",adapter5," ", adapter3," ",fastq[1]," ", threads," ", fastq.folder," ", min.length , sep="")
			   resultRun <- runDocker(group="docker", params=params)
		}
	}

	if(resultRun==0){
	  #not saving fastq files
	  cat("\nskewer step is finished\n")
	  dir.tmp <- dir(scrat_tmp.folder)
	  dir.tmp <- setdiff(dir.tmp, dir.tmp[grep("fastq$",dir.tmp)])
	  for(i in dir.tmp){
	    system(paste("cp ", scrat_tmp.folder, "/", i, " " , fastq.folder, sep=""))
	  }
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

	#saving log and removing docker container
	container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
#	system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker logs ", container.id, " >& ","skewer_",substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))

  #removing temporary folder
	cat("\n\nRemoving trimmed temporary file ....\n")
	system(paste("rm -R ",scrat_tmp.folder))
	system(paste("rm  ",fastq.folder,"/dockerID", sep=""))
	system(paste("rm  ",fastq.folder,"/tempFolderID", sep=""))

	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
	setwd(home)
}
