#' @title Function to produce an interactive heatmap using plot.ly
#' @description This function generates an heatmap using a count table and a specific gene list
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param count.table, a character string indicating the path of the counts table file
#' @param gene.list, a character string indicating the path of the file containing the genes to include in the heatmap
#' @param output.folder, a character string indicating the path of the output folder
#' @param separator, a character string indicating the separator character in the count table. Allowed characters are TAB ('\t'), COMMA (',') and SPACE (' ')
#' @param status, a character string, 'raw' if the data are raw counts, 'log' otherwise
#' @param lower.range, the lower range of signal in the heatmap
#' @param upper.range, the upper range of signal in the heatmap
#' @author Nicola Licheri, nicola [dot] licheri [at] unito [dot] it, University of Turin
#'
#' @return A html file containing the interactive heatmap produced using plot.ly
#' @examples
#'\dontrun{
#'
#' }
#' @export
heatmaply <- function(group=c("docker", "sudo"), scratch.folder, count.table, gene.list, output.folder,
                      separator=c("TAB", "COMMA", "SPACE"), status=c("raw", "log"),
                      lower.range=NA, upper.range=NA) {

  #running time 1
  ptm <- proc.time()

  #obtaining absolute paths
  count.table <- normalizePath(count.table)
  gene.list <- normalizePath(gene.list)
  scratch.folder <- normalizePath(scratch.folder)
  output.folder <- normalizePath(output.folder)

  data.folder <- output.folder

  #storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that ",scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }

  if (is.na(lower.range) || is.na(upper.range)) {
      lower.range = 0
      upper.range = 0
  }

  params <- paste(
    "--cidfile", paste0(data.folder, "/dockerID"),
    "-v", paste0(count.table, ":/data/in/count_table"),
    "-v", paste0(gene.list, ":/data/in/gene_list"),
    "-v", paste0(output.folder, ":/data/out"),
    "-v", paste0(scratch.folder, ":/scratch"),
    "-d", "docker.io/cursecatcher/heatmap", "/bin/bash /bin/heatmap.sh", count.table, separator, status, lower.range, upper.range
  )
  resultRun <- runDocker(group=group, params=params)

  if (resultRun == 0) {
    cat("Heatmaply terminated successfully.\n")
  }

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", "heatmaply_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
