#' @title Run docker container
#' @description This is an internal function executing a docker container. Not to be used by users. 
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param params, a character string containing all parameters needed to tun the docker container
#' @param container, a character string downloading the docker container. If no value is inserted the container is already present on the local server
#' @return NULL if success, 1 if parameters are missing, 2 if the group is neither sudo or docker. In case of 1 or 2 the docker execution is aborted
#' @author Raffaele Calogero
#' 
#' @examples
#'\dontrun{
##'     #running runDocker
#'      runDocker(group="docker", container="docker.io/rcaloger/bwa.2017.01", params=NULL)
#'     
#' }
#' @export
runDocker <- function(group="docker",container=NULL, params=NULL){
     if(is.null(container)){
        cat("\nNo url to download the container was provided. Thus, container should be already located in the server\n")
     }
     if(is.null(params)){
       cat("\nNo parameters where provided!\n")
       return(1)
     }
  
  # to obtain the Docker ID by file
    if (file.exists("dockerID")){ 
        cat("\n\nDocker does not start, there is already a docker container running che dockerID file!!!\n\n")
        return(2)
    }
  
   if(group=="sudo"){
   #      system(paste("sudo docker pull ",container, sep=""))
         system(paste("sudo docker run --privileged=true ",params, sep=""))
     }else if(group=="docker"){
   #      system(paste("docker pull ",container, sep=""))
         system(paste("docker run --privileged=true ",params, sep=""))
     }else{
       cat("\nThe group provides is neither sudo or docker\n")
       return(2)
     }
     
 
  dockerid=readLines("dockerID", warn = FALSE)
  cat("\nDocker ID is:\n",substr(dockerid,1, 12),"\n")
  ## to obtain the Docker ID by file
  
  ## to check the Docker container status
  dockerStatus=system(paste("docker inspect -f {{.State.Running}}",dockerid),intern= T)
  cat("\n\nBefore while, docker status: ",dockerStatus,"\n")
  while(dockerStatus=="true"){
    Sys.sleep(10);
    dockerStatus=system(paste("docker inspect -f {{.State.Running}}",dockerid),intern= T)
    cat(".")
  }
  cat(".\n\n")
  ## to check the Docker container status
  dockerExit <- system("docker ps -a", intern= T)
  dockerExit <- dockerExit[grep(dockerid,dockerExit)]
  cat("\n",dockerExit,"\n")

  dockerExit.tmp <- unlist(strsplit(dockerExit, "ago"))
  if(length(grep("Exited", dockerExit.tmp))>0){
    dockerExit.tmp <- dockerExit.tmp[grep("Exited", dockerExit.tmp)]
    dockerExit.tmp <- gsub("  ", " ", dockerExit.tmp)
    dockerExit.tmp <- unlist(strsplit(dockerExit.tmp, " "))
    dockerExit.tmp <- dockerExit.tmp[grep("Exited", dockerExit.tmp)+1]
    dockerExit.tmp <- sub('\\(', "", dockerExit.tmp)
    dockerExit.tmp <- sub('\\)', "", dockerExit.tmp)
    if(as.numeric(dockerExit.tmp)!=0){
      system(paste("docker logs ", substr(dockerid,1,12), " &> ", substr(dockerid,1,12),"_error.log", sep=""))
      cat(paste("\nDocker container ", substr(dockerid,1,12), " had exit different from 0\n", sep=""))
      cat("\nExecution is interrupted\n")
      cat(paste("Please send to raffaele.calogero@unito.it this error: Docker failed exit 0,\n
                the description of the function you were using and the following error log file,\n
                which is saved in your working folder:\n", substr(dockerid,1,12),"_error.log\n", sep=""))
      return("Docker failed exit 0")
    }
  }
  
  return(dockerStatus)
  
}
