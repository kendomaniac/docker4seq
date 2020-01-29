#' @title Running CircHunter circRNA classification module
#' @description This function executes the docker container circhunter by running the circRNA classification module of CircHunter starting from a set of circRNAs. For CircHunter algorithm detail please refer to: https://github.com/carlo-deintinis/circhunter/tree/master/CircHunter.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param circrna.data, string indicating the path to the list of circRNAs
#' @param exon.data, string indicating the path to the exon annotation file
#' @param isoform.data, string indicating the path to the isoform annotation file
#' @param assembly, string indicating the reference human genome assembly. Compatible assemblies: hg19 (default), hg18, hg38, mm9, mm10, rn6, dm6, ce11
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return Two tab-delimited tables reporting the transcript- and gene-level classification of a list of circRNAs
#' @examples
#'\dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #retrieve the example data
#'     system("unzip master.zip")
#'
#'     #running the circrnaClassification function
#'     circrnaClassification(group="docker", scratch.folder="/data/scratch", circrna.data=paste(getwd(),"/circhunter-master/CircHunter/toyexample/toy_circRNA", sep=""), exon.data=paste(getwd(),"/circhunter-master/CircHunter/toyexample/toy_genome", sep=""), isoform.data=paste(getwd(),"/circhunter-master/CircHunter/toyexample/toy_isoformdata", sep=""), assembly="hg19")
#'
#' }
#' @export


circrnaClassification <- function(group=c("sudo","docker"), scratch.folder, circrna.data, exon.data, isoform.data, assembly="hg19") {

  #running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  circrna.data <- normalizePath(circrna.data)
  exon.data <- normalizePath(exon.data)
  isoform.data <- normalizePath(isoform.data)

  #obtaining output data folder
  data.folder <- dirname(circrna.data)

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


  #check if input files exist
  if (!file.exists(circrna.data)) {
    cat(paste("\nIt seems that the ",circrna.data, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (!file.exists(exon.data)) {
    cat(paste("\nIt seems that the ",exon.data, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (!file.exists(isoform.data)) {
    cat(paste("\nIt seems that the ",isoform.data, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (!(assembly %in% c("hg19", "hg18", "hg38", "mm9", "mm10", "rn6", "dm6", "ce11"))) {
    cat(paste("\nThe given assembly is not supported\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  #check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the", scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }

  #executing the docker job
  params <- paste(
      "--cidfile", paste0(data.folder, "/dockerID"),
      "-v", paste0(scratch.folder, ":/scratch"),
      "-v", paste0(data.folder, ":/data/out"),
      "-v", paste0(isoform.data, ":/data/isoformdata"),
      "-v", paste0(exon.data, ":/data/genome"),
      "-v", paste0(circrna.data, ":/data/circRNA"),
      "-d docker.io/repbioinfo/docker4circ.2019.02 Rscript /scripts/circhunter/circhunter.R",
      "--classification",
      "-as", assembly,
      "-v", version,
      "-sg", #exon.data,
      "-id", #isoform.data,
	  "-of" #output_folder
  )

  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nThe circRNAs classification analysis is finished\n")
  }

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("circRNAs classification user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNAs classification system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNAs classification elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }
  else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNAs classification system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("circRNAs classification elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/circrnaClassification_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
