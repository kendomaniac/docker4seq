#' @title Running CASC step2 identification of the optimal number of clusters
#' @description This function executes CASC step2 identification of the optimal number of clusters
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of csv  file of cells log10 counts
#' @param totIdentity, an integer between 1 to 100 which indicates the percentage of identity required to consider the results of two permutations identical.
#' @param clusterIdentity, an integer between 1 to 100 which indicates the percentage of identity required to consider two clusters identical.
#' @param k.min, min number of clusters, should be the same of those passes to cascStep1.
#' @param k.max, min number of clusters, should be the same of those passes to cascStep1.
#' @return a folder with .....
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/log10_singlecells_counts.csv.gz")
#'     system("gzip -d log10_singlecells_counts.csv.gz")
#'     #running casc step1
#'     cascStep2(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#'     totIdentity=80, clusterIdentity=80, k.min=2, k.max=4, counts.matrix="log10_singlecells_counts.csv")
#' }
#'
#' @export
cascStep2 <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), totIdentity=80, clusterIdentity=80, k.min, k.max, counts.matrix){

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  setwd(data.folder)
  #########check scratch folder exist###########
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    return(3)
  }
  #############################################
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scratch.folder, tmp.folder))
  system(paste("cp -R Data ", scrat_tmp.folder,sep=""))
  system(paste("cp -R Result ", scrat_tmp.folder,sep=""))
  system(paste("cp -R output ", scrat_tmp.folder,sep=""))


  if(group=="sudo"){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/Analysis_cra.R ",totIdentity," ",clusterIdentity," ",k.min," ",k.max," ",counts.matrix, sep="")
    runDocker(group="sudo",container="docker.io/rcaloger/casc", params=params)
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/Analysis_cra.R ",totIdentity," ",clusterIdentity," ",k.min," ",k.max," ",counts.matrix, sep="")
    runDocker(group="docker",container="docker.io/rcaloger/casc", params=params)
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
    tmp.run[length(tmp.run)+1] <- paste("casc step2 user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step2 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step2 elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("casc step2 user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step2 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step2 elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }
#  system(paste("cp -R ", scrat_tmp.folder, "/output ", data.folder, sep=""))
  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
}
