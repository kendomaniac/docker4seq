#' @title Running CASC step1 identification of the optimal number of clusters
#' @description This function executes CASC step1 identification of the optimal number of clusters
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of csv  file of cells log10 counts
#' @param permutations, a integer indicating how main permutations of SIMLR are going to be executed.
#' @param blocks.permutations, a integer indicating how many SIMLR permutation are going to be run in parallel. e.g. 10 indicates that 100 permutatin will be run in 10 groups of 10 permutations.
#' @param core, a integer between 0 to 1 to define the fraction of cores to be used by SIMLR, default 0, other values will generate an error in specific hadrware configurations.
#' @param bootstrap.fraction, a integer between 1 and 100, which indicate the fraction of cell to be removed during boostrap.
#' @param k.min, min number of clusters.
#' @param k.max, min number of clusters.
#' @return a folder with the name of the cell data set under analysis, e.g. log10_singlecells_counts. In this folder there are a set of folder named by the number of k clusters in use. A file named SilhouetteValue.csv for cluster evaluation and a pdf ViolinPlot.pdf which is the representation of the SilhouetteValue.csv. best clusters are represented by a SilhouetteValue distribution skewed versus 1 values.
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/log10_singlecells_counts.csv.gz")
#'     system("gzip -d log10_singlecells_counts.csv.gz")
#'     #running casc step1
#'     cascStep1(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#'     counts.matrix="log10_singlecells_counts.csv", permutations=20, blocks.permutations=2, core=0, bootstrap.fraction=10, k.min=2, k.max=4)
#' }
#'
#' @export
cascStep1 <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), counts.matrix, permutations=100, blocks.permutations=10, core=0, bootstrap.fraction=10, k.min, k.max){

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
  dir.create(file.path(scratch.folder, tmp.folder,"/Data"))
  system(paste("cp ",counts.matrix, " ", scrat_tmp.folder,"/Data",sep=""))

#  writeLines(file.path(scratch.folder, tmp.folder),"path.txt")

#  system(paste(path.package(package="docker4seq"),"/inst/script/1_perm.sh ",counts.matrix," ",permutations," ",blocks.permutations," ",core," ",bootstrap.fraction," ",k.min," ",k.max, sep=""))
#D=$(docker run -i -t -d -v /Users/raffaelecalogero/Desktop/single-celll_Buettner:/data -v /Users/raffaelecalogero/Desktop/scratch:/scratch docker.io/rcaloger/casc /bin/bash)
#Rscript /home/CASC/Main_cra.R log10_singlecells_counts.csv 100 10 0 10 2 4

  if(group=="sudo"){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/Main_cra.R ",counts.matrix," ",permutations," ",blocks.permutations," ",core," ",bootstrap.fraction," ",k.min," ",k.max, sep="")
    runDocker(group="sudo",container="docker.io/rcaloger/casc", params=params)
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/Main_cra.R ",counts.matrix," ",permutations," ",blocks.permutations," ",core," ",bootstrap.fraction," ",k.min," ",k.max, sep="")
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
    tmp.run[length(tmp.run)+1] <- paste("casc step1 user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step1 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step1 elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("casc user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step1 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc step1 elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))

}
