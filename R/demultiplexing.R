#' @title Generating running bcl2fastq
#' @description This function executes the Illumina bcl2fastq program
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param data.folder, a character string indicating the Illumina folder where the Samplesheet.csv is located, and example of Samplesheet.cvs is in inst/examples folder
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param threads, a number indicating the number of cores to be used from the application
#' @author Raffaele Calogero
#'
#' @return Fastq files
#' @examples
#'\dontrun{
#'     #running rsemstar index for human
#'     demultiplexing(group="docker",
#'     data.folder="/home/calogero/Documents/data/lollini/3a_run/170712_NB501050_0097_AH3FGNBGX3",
#'     scratch.folder="/data/scratch", threads=24)
#'
#' }
#' @export
demultiplexing <- function(group=c("sudo","docker"),  data.folder, scratch.folder, threads=8){
  home <- getwd()
  #########check scratch folder exist###########
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, "folder does not exist.\n"))
    return(1)
  }
  #############################################
  setwd(data.folder)
  tmp <- strsplit(data.folder, "/")
  tmp <- unlist(tmp)
  main.folder <- paste(tmp[(1:length(tmp)-1)], collapse="/")
  illumina.folder <- tmp[length(tmp)]
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  
  #creating a tmp folder in scratch
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  if(length(dir(data.folder)[grep("run.info",dir(data.folder))]) == 0){
    system(paste("touch ", scrat_tmp.folder,"/run.info",sep=""))
  }else{
    system(paste("mv run.info ", scrat_tmp.folder, sep=""))
  }
  
  #getting in the tmp folder in scratch folder
  setwd(main.folder)
  system(paste("cp -R ", data.folder, " ", scrat_tmp.folder, sep=""))

	if(group=="sudo"){
	      params <- paste("--cidfile ", main.folder,"/dockerID -v ", main.folder,":/data.folder -v ", scrat_tmp.folder,":/data/scratch -d docker.io/repbioinfo/demultiplexing.2017.01 sh /bin/demultiplexing.sh ",illumina.folder," "," ",threads, sep="")
	      resultRun <- runDocker(group="sudo",container="docker.io/repbioinfo/demultiplexing.2017.01", params=params)
	}else{
	  params <- paste("--cidfile ", main.folder,"/dockerID -v ", main.folder,":/data.folder -v ", scrat_tmp.folder,":/data/scratch -d docker.io/repbioinfo/demultiplexing.2017.01 sh /bin/demultiplexing.sh ",illumina.folder," "," ",threads, sep="")
	  resultRun <- runDocker(group="docker",container="docker.io/repbioinfo/demultiplexing.2017.01", params=params)
	}

     #running time 2
    system(paste("mv ",  scrat_tmp.folder,"/",illumina.folder,"/Data/Intensities/BaseCalls/*.fastq.gz ", main.folder, sep=""))
    system(paste("mv ",  scrat_tmp.folder,"/",illumina.folder,"/Data/Intensities/BaseCalls/Reports/html ", main.folder, sep=""))
    system(paste("mv ",  scrat_tmp.folder,"/",illumina.folder,"/Data/Intensities/BaseCalls/Stats ", main.folder, sep=""))
  
    system(paste("mv ", scrat_tmp.folder,"/run.info ",main.folder, sep=""))
    ptm <- proc.time() - ptm
    con <- file(paste(main.folder,"run.info", sep="/"), "r")
    tmp.run <- readLines(con)
    close(con)

    tmp.run <- NULL
    tmp.run[length(tmp.run)+1] <- paste("demultiplexing user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("demultiplexing system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("demultiplexing elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run, paste(main.folder,"run.info", sep="/"))

    #saving log and removing docker container
    container.id <- readLines(paste(main.folder,"/dockerID", sep=""), warn = FALSE)
    system(paste("docker logs ", container.id, " >& ", "demultiplexing_",substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))

    #running time 2
    system(paste("rm ",main.folder,"/dockerID", sep=""))
    system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",main.folder, sep=""))
    setwd(home)
}

