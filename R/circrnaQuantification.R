#' @title Running CircHunter circRNA quantification module
#' @description This function executes the docker container circhunter by running the circRNA quantification module of CircHunter to quantify the level of expression of a set of circRNA BS sequences in a given RNA-Seq experiment. For CircHunter algorithm detail please refer to: https://github.com/carlo-deintinis/circhunter/tree/master/CircHunter.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param rnaseq.data, string indicating the path to the fastq file of the RNA-Seq dataset to analyse
#' @param backsplicing_junctions.data, string indicating the path to the fasta file of the circRNA back-splicing sequences to search in the RNA-Seq dataset
#' @param hc.params, vector of six parameters to set the analysis. The element of the vector indicate in order: the k-mer size, the thread number, the dimension of the hash table, the dimension of the collision list, the number of k-mers that must be matched to the sequence to consider the sequence itself as represented in the RNA-Seq data, and the number of perfect matches required in the k-mer to consider it matched to a sequence.
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return A count table reporting the number of RNA-Seq reads supporting specific circRNA back-splice junctions
#' @examples
#'\dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #'     system("unzip master.zip")
#'     system("gzip ./circhunter-master/CircHunter/data/test_rna-seq.fastq.gz")
#'
#'     #running the circrnaQuantification function
#'     circrnaQuantification(group="docker", scratch.folder="/data/scratch", rnaseq.data=paste(getwd,"/circhunter-master/CircHunter/data/test_rna-seq.fastq.gz", sep=""), backsplicing_junctions.data=paste(getwd,"/circhunter-master/CircHunter/data/CRC_circRNA_backsplicing_sequences.fasta", sep=""), hc.params=c(27, 40, 1000000, 1000000, 17, 30))
#'
#' }
#' @export


circrnaQuantification <- function(group = c("sudo", "docker"), scratch.folder, rnaseq.data, backsplicing_junctions.data, hc.params) {


  # running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  rnaseq.data <- normalizePath(rnaseq.data)
  backsplicing_junctions.data <- normalizePath(backsplicing_junctions.data)

  # obtaining output data folder
  data.folder <- dirname(rnaseq.data)

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


  # check if input files exist
  if (!file.exists(rnaseq.data)) {
    cat(paste("\nIt seems that the ", rnaseq.data, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (!file.exists(backsplicing_junctions.data)) {
    cat(paste("\nIt seems that the ", backsplicing_junctions.data, " file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (length(hc.params) != 6) {
    cat(paste("\nIt seems that the provided number of HashCirc parameters is incorrect\n")) # ?
    system("echo 4 > ExitStatusFile 2>&1")
    setwd(home)
    return(4)
  }

  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the ", scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }

  #getting sample name
  samplename.rna <- strsplit(basename(rnaseq.data), "\\.")[[1]][1]

  # executing the docker job
  params <- paste(
    "--cidfile", paste0(data.folder, "/dockerID"),
    "-v", paste0(scratch.folder, ":/scratch"),
    "-v", paste0(data.folder, ":/data/out"),
    "-v", paste0(rnaseq.data, ":/data/rnaseq"),
    "-v", paste0(backsplicing_junctions.data, ":/data/bksj"),
    "-d docker.io/repbioinfo/docker4circ.2019.02 Rscript /scripts/circhunter/circhunter.R",
    "--readcount",
    "-of", # output_folder
    "-hc", paste(format(hc.params, scientific=FALSE), collapse=" "),
    "--samplename", samplename.rna #to set output filename
  )
  resultRun <- runDocker(group = group, params = params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nThe circRNAs quantification analysis is finished\n")
  }

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("circRNA quantification user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNA quantification system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNA quantification elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  }
  else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNA quantification system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("circRNA quantification elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste(data.folder, "/dockerID", sep = ""), warn = FALSE)
  system(paste("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/circrnaQuantification_", substr(container.id, 1, 12), ".log", sep = ""))
  system(paste("docker rm ", container.id, sep = ""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
