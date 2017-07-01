#' @title Running CASC step1 identification of the optimal number of clusters
#' @description This function executes CASC step1 identification of the optimal number of clusters
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param data.folder, a character string indicating the folder where tab separated file of cells counts is saved
#' @param counts.matrix, a character string indicating the  tab separated file of cells  counts
#' @param drop.thre, A number between 0 and 1, specifying the threshold to determine dropout values
#' @param cores, a integer specifying the number of cores used for parallel computation.
#' @return ...
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#'     system("gzip -d singlecells_counts.txt.gz")
#'     cascImpute(group="docker", data.folder=getwd(), counts.matrix="singlecells_counts.txt", drop.thre=0.5, cores=8)
#' }
#'
#' @export
cascImpute <- function(group=c("sudo","docker"), data.folder, counts.matrix, drop.thre, cores){

  if(group=="sudo"){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder, ":/data -d docker.io/rcaloger/r340.2017.01 Rscript /bin/scimpute.R ",counts.matrix," ",drop.thre," ",cores, sep="")
    runDocker(group="sudo",container="docker.io/rcaloger/r340.2017.01", params=params)
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder, ":/data -d docker.io/rcaloger/r340.2017.01 Rscript /bin/scimpute.R ",counts.matrix," ",drop.thre," ",cores, sep="")
    runDocker(group="docker",container="docker.io/rcaloger/r340.2017.01", params=params)
  }

  out <- "xxxx"
  #waiting for the end of the container work
  while(out != "anno.info"){
    Sys.sleep(10)
    cat(".")
    out.tmp <- dir(file.path(data.folder))
    out.tmp <- out.tmp[grep("anno.info",out.tmp)]
    if(length(out.tmp)>0){
      out <- "anno.info"
    }
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("casc impute user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc impute system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc impute elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("casc impute user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc impute system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc impute elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))


}
