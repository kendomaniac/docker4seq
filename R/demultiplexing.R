#' @title Generating running bcl2fastq
#' @description This function executes the Illumina bcl2fastq program
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param data.folder, a character string indicating the Illumina folder where the Samplesheet.csv is located, and example of Samplesheet.cvs is in inst/examples folder
#' @param threads, a number indicating the number of cores to be used from the application
#'
#' @return Fastq files
#' @examples
#'\dontrun{
#'     #running rsemstar index for human
#'     demultiplexing(group="docker",
#'     data.folder="/home/calogero/Documents/data/lollini/3a_run/170712_NB501050_0097_AH3FGNBGX3",
#'     threads=24)
#'
#' }
#' @export
demultiplexing <- function(group=c("sudo","docker"),  data.folder, threads=8){
  tmp <- strsplit(data.folder, "/")
  tmp <- unlist(tmp)
  main.folder <- paste(tmp[(1:length(tmp)-1)], collapse="/")
  illumina.folder <- tmp[length(tmp)]
  #running time 1
  ptm <- proc.time()
  #running time 1

  #########check scratch folder exist###########
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, "folder does not exist.\n"))
    return(1)
  }

  #storing the position of the home folder
  home <- getwd()
  setwd(main.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }

  #############################################
  cat("\nsetting as working dir the genome folder and running bwa docker container\n")
  params <- paste("--cidfile ", main.folder,"/dockerID -v ", main.folder,":/data/scratch"," -d docker.io/repbioinfo/demultiplexing.2017.01 sh /bin/demultiplexing.sh ",illumina.folder," "," ",threads, sep="")
  resultRun=runDocker(group=group, params=params)

	 if(resultRun==0){
	    cat("\nDemultiplexing is finished\n")
	  }




  #running time 2
  system(paste("mv ",  data.folder,"/Data/Intensities/BaseCalls/*.fastq.gz ",main.folder, sep=""))
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
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  #running time 2
  system(paste("rm ",main.folder,"/dockerID", sep=""))
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",main.folder, sep=""))
  setwd(home)
}
