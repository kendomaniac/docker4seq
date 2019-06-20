#' @title Function to merge different circRNA lists from CIRI 2
#' @description This function executes the docker container ciri2merge by running the merge of different lists of circRNAs predicted by CIRI2  following a sample data files provided by the user. The function executes also a filter based on the number of back-splicing reads computed in each experiment and across replicates of the same biological condition.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param data.folder, a character string indicating the data folder where the file to merge are located
#' @param samples.ids, a character vector indicating the identifiers of the samples
#' @param covariates, a character vector indicating the classes of the samples
#' @param covariate.order, a character vector indicating a vector reporting the covariate classes ordered as desidered in the output file
#' @param extension, a character string indicating the filename extension of the files that have to merge
#' @param column_index, an integer value > 1 indicating which column values have to been reported in the output file
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return Two tab-delimited tables reporting the BS supporting reads and the coordinates of the filtered circRNAs are reported
#' @examples
#'\dontrun{
#'
#' }
#' @export

mergeData <- function(group = c("sudo", "docker"), scratch.folder, data.folder,
    samples.ids, covariates, covariate.order, extension, column_index) {


  # running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  data.folder <- normalizePath(data.folder)

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

  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the ", scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }

  # check if each sample is associated to a covariate and viceversa
  if (length(samples.ids) != length(covariates)) {
    cat("\nSamples and covariates lists must have the same length.\n")
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  #validate column index
  if (column_index <= 1) {
    cat("\nThe column index must be greater than 1.\n")
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  # executing the docker job
  params <- paste(
      "--cidfile", paste0(data.folder, "/dockerID"),
      "-v", paste0(scratch.folder, ":/scratch"),
      "-v", paste0(data.folder, ":/data/"),
      "-d docker.io/repbioinfo/docker4circ.2019.01 merge_data",
      "--samples", paste(samples.ids, collapse = " "),
      "--cov", paste(covariates, collapse = " "),
      "--order", paste(covariate.order, collapse = " "),
      "--col", column_index,
      "--ext", extension
  )
  resultRun <- runDocker(group = group, params = params)

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste(data.folder, "/dockerID", sep = ""), warn = FALSE)
  system(paste("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/mergeData_", substr(container.id, 1, 12), ".log", sep = ""))
  system(paste("docker rm ", container.id, sep = ""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ", paste(path.package(package = "docker4seq"), "containers/containers.txt", sep = "/"), " ", data.folder, sep = ""))
  setwd(home)
}
