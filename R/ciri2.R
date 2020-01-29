#' @title Running CIRI v2 tool for circRNAs prediction
#' @description This function executes the docker container ciri2 where CIRI v2.0.6 is installed and it provides the list of circRNAs predicted from a RNA-Seq experiment. For CIRI 2 tool detail refer to: "Gao, Y., Zhang, J., & Zhao, F. (2017). Circular RNA identification based on multiple seed matching. Brief Bioinform. 2018 Sep 28;19(5):803-810."
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param sam.file, a character string indicating the path to the RNA-Seq alignment SAM file from BWA
#' @param genome.file, a character string indicating the path to the Fasta file of the reference genomic sequence (it should be the same reference indexed for the BWA alignment)
#' @param annotation.file, a character string indicating the path to the GTF/GFF file reporting the reference gene annotations
#' @param max.span, an integer reporting the maximum spanning distance of a circRNA (default = 200000 bp)
#' @param stringency.value, the selected stringency level of the analysis. Three possible options are available: "high" (high stringency, default), in which CIRI2 only provides circRNAs supported by more than 2 distinct Paired Chiastic Clipping (PCC) signals; "low" (low stringency), CIRI2 only provides circRNAs supported by more than 2 junction reads; "zero", CIRI2 provides all circRNAs regardless junction read counts or PCC signals
#' @param quality.threshold, integer indicating the threshold for mapping quality of each segment of junction reads (default=10)
#' @param threads, integer indicating the number of threads used for the analysis (default=1)
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return The list of CIRI 2 predicted circRNAs
#' @examples
#'\dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://sourceforge.net/projects/ciri/files/CIRI2/CIRI_v2.0.6.zip") #retrieve the example data
#'     system("unzip CIRI_v2.0.6.zip")
#'
#'     #running ciri2 function
#'     ciri2(group="docker", scratch.folder="/data/scratch", sam.file=paste(getwd(),"/CIRI_v2.0.6/data/sample.sam", sep=""), genome.file=paste(getwd(),"/CIRI_v2.0.6/data/chr1.fa", sep=""), annotation.file="", max.span=200000, stringency.value="high", quality.threshold=10, threads=1)
#
#' }
#'
#' @export

ciri2 <- function(group = c("sudo", "docker"), scratch.folder, sam.file,
    genome.file, annotation.file = NA, max.span = 200000,
    stringency.value = c("high", "low", "zero"), quality.threshold = 10, threads = 1) {

  # running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  sam.file <- normalizePath(sam.file)
  genome.file <- normalizePath(genome.file)

  if (!is.na(annotation.file)) {
      annotation.file <- normalizePath(annotation.file)
      tokens <- strsplit(annotation.file, "\\.")[[1]]
      annotation_extension <- tokens[length(tokens)]
  }

  data.folder <- dirname(sam.file)

  # setting the data.folder as working folder
  if (!file.exists(data.folder)) {
    cat(paste("\nIt seems that the", data.folder, "folder does not exist\n"))
    return(2)
  }

  # storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  # initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  # checking input files exist
  if (!file.exists(sam.file)) {
    cat(paste("\nIt seems that the", sam.file, "file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }
  if (!file.exists(genome.file)) {
    cat(paste("\nIt seems that the", genome.file, "file does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  # checking if the user provided the annotation file
  if (!is.na(annotation.file) && !file.exists(annotation.file)) {
      cat(paste("\nIt seems that the", annotation.file, "file does not exist\n"))
      system("echo 2 > ExitStatusFile 2>&1")
      setwd(home)
      return(2)
  }

  #checking strigency value
  available.options <- c("high", "low", "zero")
  stringency <- stringency.value
  if (!stringency.value %in% available.options) {
      cat(paste("\nStrigency value in not allowed. Available options are:",
        paste(available.options, collapse=" "))
      )
      system("echo 5 > ExitStatusFile 2>&1")
      setwd(home)
      return(5)
  }

  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the", scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }

  # executing the docker job
  params <- paste(
    "--cidfile", paste0(data.folder, "/dockerID"),
    "-v", paste0(scratch.folder, ":/scratch"),
    "-v", paste0(sam.file, ":/data/samfile"),
    "-v", paste0(genome.file, ":/data/reference"),
    "-v", paste0(data.folder, ":/data/out"),
    ifelse(!is.na(annotation.file),
        paste("-v", paste0(annotation.file, ":/data/annotation.", annotation_extension)),
        ""
    ),
    "-d docker.io/repbioinfo/docker4circ.2019.02 python3 /ciri2/docker4ciri.py ciri2",
    "--strigency", stringency,
    "-S", format(max.span, scientific=FALSE),
    "-T", threads,
    "-U", quality.threshold,
    ifelse(!is.na(annotation.file),
        "--anno",
        ""
    )
  )
  resultRun <- runDocker(group = group, params = params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nCIRI2 analysis is finished\n")
  }

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste0(data.folder, "/dockerID"), warn = FALSE)
  system(paste0("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/ciri2_", substr(container.id, 1, 12), ".log"))
  system(paste("docker rm", container.id))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp", paste(path.package(package = "docker4seq"), "containers/containers.txt", sep = "/CIRI2_"), data.folder))
  setwd(home)
}
