#' @title Running circRNA reformat script 
#' @description This function executes the docker container docker4circ to reformat circRNA prediction file produced by a certain tool in a common format to further analysis.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param input.folder, string indicating the path of the folder which contains the input files to be reformatted
#' @param output.folder, string indicating the path to the reformatted circRNA prediction files
#' @param tool, string indicating the tool used to create the input files. Supported tools are: acfs, circexplorer, circexplorer2, circrnafinder, ciri, ciri2, dcc, findcirc2, knife, starchip, uroborus
#' @author Nicola Licheri
#'
#' @return A circRNA list reformatted in a tab-delimited file storing the circRNA genomic coordinates
#' @examples
#'\dontrun{
#' }
#' @export


circrnaReformat <- function(group=c("sudo", "docker"), scratch.folder, input.folder, output.folder, tool = c('acfs', 'circexplorer', 'circexplorer2', 'circrnafinder', 'ciri', 'ciri2', 'dcc', 'findcirc2', 'knife', 'starchip', 'uroborus')) {
  
  #running time 1
  ptm <- proc.time()
  
  supported_tools = c('acfs', 'circexplorer', 'circexplorer2', 'circrnafinder', 'ciri', 'ciri2', 'dcc', 'findcirc2', 'knife', 'starchip', 'uroborus')

  #obtaining absolute paths
  scratch.folder <- normalizePath(scratch.folder)
  input.folder <- normalizePath(input.folder)
  output.folder <- normalizePath(output.folder)
  
  data.folder <- output.folder
  
  #storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that", scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  
  #check if output folder exist
  if (!file.exists(output.folder)) {
    cat(paste("\nIt seems that", output.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  
  #check if input data actually exist
  if (!file.exists(input.folder)) {
    cat(paste("\nIt seems that", input.folder, "file does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  if (!tolower(tool) %in% supported_tools) {
    cat(paste("\nThe tool", tool, "is not supported yet\n"))
    system("echo 4 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(4)
  }
  
  
  
  params <- paste(
    "--cidfile", paste0(data.folder, "/dockerID"),
    "-v", paste0(input.folder, ":/data/input"), 
    "-v", paste0(output.folder, ":/data/out"), 
    "docker.io/repbioinfo/docker4circ.2019.02", 
    "python3", "/ciri2/docker4ciri.py", "reformat", "-t", tool
  )
  resultRun <- runDocker(group=group, params=params)
  
  if (resultRun == 0) {
    cat("circrnaReformat terminated successfully.\n")
  } else {
    cat("circrnaReformat terminated with errors.\n")
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
  system(paste0("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", "circrnaReformat_", substr(container.id,1,12),".log"))
  system(paste("docker rm", container.id))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}