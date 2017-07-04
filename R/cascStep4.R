#' @title Running CASC step4, step1 of feature selection
#' @description This function executes CASC step1 identification of the optimal number of clusters
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of csv  file of cells log10 counts
#' @param k, optimal number of clusters.
#' @param dispersion.min, min dispersion.
#' @param dispersion.max, max dispersion.
#' @param specific.genes, character with two option:  n, y
#' @return ....
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/log10_singlecells_counts.csv.gz")
#'     system("gzip -d log10_singlecells_counts.csv.gz")
#'     # without parameter range
#'     cascStep4(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#'     counts.matrix="log10_singlecells_counts.csv",  k=3, dispersion.min=NULL, dispersion.max=NULL, specific.genes="n")
#'     # indicating a parameter range
#'     cascStep4(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#'     counts.matrix="log10_singlecells_counts.csv",  k=3, dispersion.min=2, dispersion.max=10, specific.genes="n")
#' }
#'
#' @export
cascStep4 <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), counts.matrix, k, dispersion.min=NULL, dispersion.max=NULL, specific.genes=c("n", "y")){

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
#counts.matrix="log10_singlecells_counts.csv",  k=3, dispersion.min=2, dispersion.max=10, specific.genes="n"
  if(is.null(dispersion.min) | is.null(dispersion.max)){
    if(group=="sudo"){
        params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/IndexOfDispersion_cra.R ",counts.matrix," ",k," ",specific.genes, sep="")
        runDocker(group="sudo",container="docker.io/rcaloger/casc", params=params)
    }else{
       params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/IndexOfDispersion_cra.R ",counts.matrix," ",k," ",specific.genes, sep="")
      runDocker(group="docker",container="docker.io/rcaloger/casc", params=params)
    }
  }else{
    if(group=="sudo"){
      params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/IndexOfDispersion_cra.R ",counts.matrix," ",k," ",dispersion.min," ",dispersion.max," ",specific.genes, sep="")
      runDocker(group="sudo",container="docker.io/rcaloger/casc", params=params)
    }else{
      params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/casc Rscript /home/CASC/IndexOfDispersion_cra.R ",counts.matrix," ",k," ",dispersion.min," ",dispersion.max," ",specific.genes, sep="")
      runDocker(group="docker",container="docker.io/rcaloger/casc", params=params)
    }
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
#  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))

}
