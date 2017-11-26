#' @title Download for the first time all containers embedded in the workflows
#' @description This is a functin that preapre the docker environment to be used for the first time the docker4seq is installed.
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param containers.file, a character string with the name of the file which indicate which are the initial set of containers to be downloaded. Initally the set is given by a file located in the folder containers of docker4seq package.
#' @author Raffaele Calogero
#' 
#' @examples
#'\dontrun{
##'     #running runDocker
#'      downloadContainers(group="docker", containers.file)
#'
#' }
#' @export
downloadContainers <- function(group="docker", containers.file=NULL){
   if(is.null(containers.file)){
     containers.file=paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")
   }
   containers <- readLines(containers.file)
   for(i in containers){
     if(group=="sudo"){
       system(paste("sudo docker pull ",i, sep=""))
     }else if(group=="docker"){
       system(paste("docker pull ",i, sep=""))
     }else{
       cat("\nThe group provides is neither sudo or docker\n")
       return(2)
     }
   }
   writeLines(containers, paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/"))
}
