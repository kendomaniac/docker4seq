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
  
  #remove dockerID under the assumption that second paramiter is dockerID file
     vec_params=strsplit(params,split=" ")
     if (file.exists(vec_params[[1]][2])){ 
        cat("\n\nFile:",vec_params[[1]][2]," found ---> It will be removed\n\n")
        system(paste("rm ",vec_params[[1]][2]))
     }
  #remove dockerID   
  
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
}