#' @title Function to merge different circRNA lists predicted from one of the supported circRNA prediction tools. 
#' @description This function executes the docker container ciri2merge by running the merge of different lists of circRNAs predicted by one of the supported tools following a sample data files provided by the user. The function executes also a filter based on the number of back-splicing reads computed in each experiment and across replicates of the same biological condition.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param data.folder, a character string indicating the data folder where the circRNA  output files are located
#' @param samples.list, a character vector indicating the identifiers of the samples
#' @param covariates.list, a character vector indicating the classes of the samples
#' @param covariate.order, a character vector indicating the order of covariates in the output files
#' @param min_reads, the minimum number of back-splicing reads supporting a circRNA and detected in at least min_reps number of biological replicates of the same experimental condition (default = 2)
#' @param min_reps, the minimum number of replicates associated with at least min_reads supporting a circRNA (default = 0)
#' @param min_avg, the average number of back-splicing reads across biological replicates of the same experimental condition that shall support a circRNA (default = 10)
#' @param used.tool, the tool used to predict the circRNAs. Supported tools are: ACFS, CIRI, CIRI2, CIRCexplorer, CIRCexplorer2, CircRNA_Finder, DCC, Find_Circ2, KNIFE, Uroborus.
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return Two tab-delimited tables reporting the BS supporting reads and the coordinates of the filtered circRNAs are reported
#' @examples
#'\dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #retrieve the example data
#'     system("unzip master.zip")
#'     system("unzip ./circhunter-master/CircHunter/data/CIRI_predictions.zip")
#'
#'     #running the circrnaMergePredictions function
#'     circrnaMergePredictions(group="docker", scratch.folder="/data/scratch", data.folder="./circhunter-master/CircHunter/data/CIRI_predictions", groups.file="./circhunter-master/CircHunter/data/CIRI_predictions/SampleData.tsv", min_reads = 2, min_reps = 2, min_avg = 10, used.tool="ciri2")
#
#' }
#' @export

circrnaMergePredictions <- function(group = c("sudo", "docker"), scratch.folder,
    data.folder, samples.list, covariates.list, covariate.order,
    min_reads = 2, min_reps = 0, min_avg = 10, 
    used.tool = c('acfs', 'ciri', 'ciri2', 'circexplorer', 'circexplorer2', 'circrnafinder', 'dcc', 'findcirc2', 'knife')) {


  supported_tools = c('acfs', 'ciri', 'ciri2', 'circexplorer', 'circexplorer2', 'circrnafinder', 'dcc', 'findcirc2', 'knife', 'uroborus')

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

  # check tool 
  if (!tolower(used.tool) %in% supported_tools) {
    cat(paste("\nIt seems that the tool", used.tool, " is not supported by \n"))
    system("echo 4 > ExitStatusFile 2>&1")
    setwd(home)
    return (4)
  }

  # check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the ", scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }

  # check if each sample is associated to a covariate and viceversa
  if (length(samples.list) != length(covariates.list)) {
    cat("\nSamples and covariates lists must have the same length.\n")
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  # executing the docker job
  params <- paste(
      "--cidfile", paste0(data.folder, "/dockerID"),
      "-v", paste0(scratch.folder, ":/scratch"),
      "-v", paste0(data.folder, ":/data/out"),
      "-d docker.io/repbioinfo/docker4circ.2019.02 python3 /ciri2/docker4ciri.py merge",
      "--samples", paste(samples.list, collapse = " "),
      "--cov", paste(covariates.list, collapse = " "),
      "--order", paste(covariate.order, collapse = " "),
      "--mr", min_reads,
      "--mrep", min_reps,
      "--avg", min_avg, 
      "--tool", used.tool
  )
  resultRun <- runDocker(group = group, params = params)

  if(resultRun==0){
    cat("\nCIRI 2 outputs merge is finished\n")
  }

  # running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info", dir)]
  if (length(dir) > 0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 merge user run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 merge system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 merge elapsed run time mins ", ptm[3] / 60, sep = "")
    writeLines(tmp.run, "run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ", ptm[1] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 merge system run time mins ", ptm[2] / 60, sep = "")
    tmp.run[length(tmp.run) + 1] <- paste("CIRI2 merge elapsed run time mins ", ptm[3] / 60, sep = "")

    writeLines(tmp.run, "run.info")
  }

  # saving log and removing docker container
  container.id <- readLines(paste(data.folder, "/dockerID", sep = ""), warn = FALSE)
  system(paste("docker logs ", substr(container.id, 1, 12), " &> ", data.folder, "/ciri2MergePredictions_", substr(container.id, 1, 12), ".log", sep = ""))
  system(paste("docker rm ", container.id, sep = ""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ", paste(path.package(package = "docker4seq"), "containers/containers.txt", sep = "/"), " ", data.folder, sep = ""))
  setwd(home)
}
