#' @title Annotation of a list of circRNAs
#' @description This function executes the docker container ciri2 in the annotation mode to overlap a list of circRNAs with the annotations from circBase, CSCD, ExoRBase, Circ2Disease, CircFunBase, and TSCD
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param ciri.file, a list of circRNAs derived from a circRNAs prediction analysis
#' @param annotation.sources, a vector of character strings indicating the circRNA databases to analyse. Compatible databases id: circbase, cscd, exorbase, circ2disease, circfunbase, tscd.
#' @param assembly, a character string indicating the reference genome assembly. The function currently work with the hg18, hg19, and hg38, mm9, and mm10 genome assemblies.
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return The annotations of a list of circRNAs from different databases
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip")
#'     system("unzip master.zip")
#'
#' # Run the circAnnotations function
#'  circAnnotations(group = "docker", scratch.folder="/data/scratch", ciri.file=paste(getwd(),"/circhunter-master/CircHunter/data/circRNA_CRC.bed", sep=""), assembly="hg19")
#'
#' circAnnotations()
#' }
#' @export


circrnaAnnotations <- function(group = c("sudo", "docker"), scratch.folder, ciri.file, annotation.sources=c("circbase", "tscd", "cscd", "exorbase", "circ2disease", "circfunbase"), assembly=c("hg18", "hg19", "hg38", "mm9", "mm10")) {

  # running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  ciri.file <- normalizePath(ciri.file)

  data.folder <- dirname(ciri.file)

  # setting the data.folder as working folder
  if (!file.exists(data.folder)) {
    cat(paste("\nIt seems that the ", data.folder, " folder does not exist\n"))
    return(2)
  }

  # storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  # initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  # checking input files exist
  if (!file.exists(ciri.file)) {
    cat(paste("\nIt seems that the ", ciri.file, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the ", scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  #check if there is at least one annotation source
  if (length(annotation.sources) < 1) {
    cat("\nIt seems that you do not specified any annotation source\n")
    system("echo 4 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(4)
  }

  # executing the docker job
  params <- paste(
      "--cidfile", paste0(data.folder, "/dockerID"),
      "-v", paste0(scratch.folder, ":/scratch"),
      "-v", paste0(ciri.file, ":/data/cirifile"),
      "-v", paste0(data.folder, ":/data/out"),
      "-d docker.io/repbioinfo/docker4circ.2019.02 python3 /ciri2/docker4ciri.py annotation",
      "-s", paste(annotation.sources, collapse=" "),
      "-v", assembly
  )
  resultRun <- runDocker(group = group, params = params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nThe circRNAs annotation analysis is finished\n")
  }

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("circRNAs annotation user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNAs annotation system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNAs annotation elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNAs annotation system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNAs annotation elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste(data.folder, "/dockerID", sep = ""), warn = FALSE)
  system(paste("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/circAnnotation_", substr(container.id, 1, 12), ".log", sep = ""))
  system(paste("docker rm ", container.id, sep = ""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ", paste(path.package(package = "docker4seq"), "containers/containers.txt", sep = "/"), " ", data.folder, sep = ""))
  setwd(home)
}
