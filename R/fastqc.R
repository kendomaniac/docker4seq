#' @title A function to handle a docker containier executing fastqc
#' @description This function executes a ubuntu docker that produces as output FASTQCstdin_fastqc.html and stdin_fastqc.zip files
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param large, boolenan if FALSE uses 10000 reads to make fastq statistics if TRUe uses 1 milion reads
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino
#'
#' @examples
#' \dontrun{
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     #running fastqc
#'     fastqc(group="docker", data.folder=getwd(), large=FALSE)
#' }
#'
#' @export
fastqc <- function(group=c("sudo","docker"), data.folder, large=FALSE){


  #storing the position of the home folder
  home <- getwd()



  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }

  setwd(data.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")


  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }


  #executing the docker job
  if(!large){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data/scratch -d docker.io/repbioinfo/r340.2017.01 bash /bin/fastqc.sh", sep="")
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data/scratch -d docker.io/repbioinfo/r340.2017.01 bash /bin/fastqc_large.sh", sep="")
  }
  
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nFastQC analysis is finished\n")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("FastQC user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("FastQC system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("FastQC elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("FastQC user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("FastQC system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("FastQC elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/fastqc_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
