#' @title Function to prepare the CircHunter reference annotations
#' @description This function executes the docker container circhunter by running the circRNA classification module of CircHunter starting from a set of circRNAs. For CircHunter algorithm detail please refer to: https://github.com/carlo-deintinis/circhunter/tree/master/CircHunter.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param data.folder, a character string indicating the data folder where the output files will be saved
#' @param assembly, string indicating the reference human genome assembly. Compatible assemblies: hg19 (default), hg18, hg38, mm9, mm10, rn6, dm6, ce11
#' @param version, Ensembl database version used for the analysis. If no version number is provided, the last version is considered
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return Two tab-delimited tables reporting the exons and the transcript isoforms annotations of an user-selected human genome assembly
#' @examples
#'\dontrun{
#'     #Download the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip")
#'     system("unzip master.zip")
#'
#'     #running the circrnaPrepareFiles function
#'     circrnaPrepareFiles(group="docker", scratch.folder="/data/scratch", data.folder="/data/output", assembly="hg19")
#'
#' }
#' @export

circrnaPrepareFiles <- function(group=c("sudo","docker"), scratch.folder, data.folder, assembly="hg19", version=NULL) {

  #running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  data.folder <- normalizePath(data.folder)

  #setting the data.folder as working folder
  if (!file.exists(data.folder)) {
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }

  #storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  #check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  
  if (!(assembly %in% c("hg19", "hg18", "hg38", "mm9", "mm10", "rn6", "dm6", "ce11"))) {
    cat(paste("\nThe given assembly is not supported\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  
  #executing the docker job
  params <- paste("--cidfile", paste0(data.folder, "/dockerID"),
                  "-v", paste0(scratch.folder, ":/scratch"),
                  "-v", paste0(data.folder, ":/data"),
                  "-d docker.io/repbioinfo/docker4circ.2019.02 Rscript /scripts/circhunter/circhunter.R",
                  "--preparedata ",
                  "-as", assembly,
                  "-of",
                  "-v", version
  )
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nThe preparation of the circRNA reference files is finished\n")
  }

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("circRNA prepare files user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNA prepare files system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNA prepare files elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }
  else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNA prepare files system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNA prepare files elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/circrnaPrepareFiles_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
