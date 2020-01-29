#' @title Running CIRI_AS tool for circRNAs structure prediction
#' @description This function executes the docker container docker4circ where CIRI_AS is installed
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param sam.file, a character string indicating the path to the RNA-Seq alignment SAM/BAM file from BWA
#' @param ciri.file, string indicating the path to the list of circRNAs
#' @param genome.file, a character string indicating the path to the Fasta file of the reference genomic sequence (it should be the same reference indexed for the BWA alignment)
#' @param annotation.file, a character string indicating the path to the GTF/GFF file reporting the reference gene annotations
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return The function returns the list of alternative circRNAs internal structures
#'
#' @examples
#' \dontrun{
#'
#'     #Download the example data
#'     system("wget https://sourceforge.net/projects/ciri/files/CIRI-AS/test_data_CIRI_AS.zip/download")
#'
#'     system("mv download test_data_CIRI_AS.zip")
#'     system("unzip test_data_CIRI_AS.zip")
#'
#'  # Run the ciriAS function
#' ciriAS(group = "docker", scratch.folder="/data/scratch", sam.file=paste(getwd,"/test_data_CIRI_AS/test.sam",sep=""), ciri.file=paste(getwd,"/test_data_CIRI_AS/test.ciri", genome.file=paste(getwd,"/test_data_CIRI_AS/chr1.fa", sep=""), annotation.file = paste(getwd,"/test_data_CIRI_AS/chr1.gtf", sep="")
#' }
#'
#' @export


ciriAS <- function(group = c("sudo", "docker"), scratch.folder, sam.file,
    ciri.file, genome.file, annotation.file = NA) {



  # running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  sam.file <- normalizePath(sam.file)
  ciri.file <- normalizePath(ciri.file)
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

  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the", scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }

  # executing the docker job
  params <- paste(
    "--cidfile", paste0(data.folder, "/dockerID"),
    "-v", paste0(scratch.folder, ":/scratch"),
    "-v", paste0(sam.file, ":/data/samfile"),
    "-v", paste0(ciri.file, ":/data/cirifile"),
    "-v", paste0(genome.file, ":/data/reference"),
    "-v", paste0(data.folder, ":/data/out"),
    ifelse(!is.na(annotation.file),
        paste("-v", paste0(annotation.file, ":/data/annotation.", annotation_extension)),
        ""
    ),
    "-d docker.io/repbioinfo/docker4circ.2019.02 python3 /ciri2/docker4ciri.py structure",
    ifelse(!is.na(annotation.file),"--anno", "")
  )
  resultRun <- runDocker(group = group, params = params)

    #waiting for the end of the container work
  if(resultRun==0){
    cat("\nCIRI AS analysis is finished\n")
  }

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("CIRI AS user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI AS system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI AS elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI AS system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI AS elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste0(data.folder, "/dockerID"), warn = FALSE)
  system(paste("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/ciriAS_", substr(container.id, 1, 12), ".log", sep = ""))
  system(paste("docker rm", container.id))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp", paste(path.package(package = "docker4seq"), "containers/containers.txt", sep = "/"), data.folder))
  setwd(home)
}
