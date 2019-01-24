#' @title A function to handle a docker containier executing MultiQC
#' @description This function executes MultiQC docker (Ewels et al., 2016. Bioinformatics. 32(19):3047-8) returning the multiqc_report.html file and the content of multiqc_data/ folder
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @author Giulio Ferrero, giulio.ferrero [at] unito [dot] it, University of Torino
#'
#' @examples
#' \dontrun{
#'     #running skeleton
#'     multiQC(group="docker", data.folder=getwd())
#' }
#'
#' @export
multiQC <- function(group=c("sudo","docker"), data.folder){


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



  tmp.folder <- gsub(":","-",gsub(" ","-",date()))

  params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder,":", data.folder, " -w ", data.folder, " docker.io/repbioinfo/multiqc.2018.01 multiqc .", sep="")
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nMultiQC analysis is finished\n")
  }

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("MultiQC user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MultiQC system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MultiQC elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("MultiQC user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MultiQC system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MultiQC elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/MultiQC_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
