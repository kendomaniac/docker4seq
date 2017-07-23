#' @title Running SCnorm  normalization
#' @description This function is a wrapper for SCnorm: robust normalization of single-cell RNA-seq data (Bacher et al. Nature Methods 2017, 14:584–586)
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of tab delimited file  file of cells un-normalized expression counts
#' @param conditions, vector of condition labels, this should correspond to the columns of the un-normalized expression matrix. If not provided data is assumed to come from same condition/batch.
#' @param outputName, specify the path and/or name of output files.
#' @param nCores, number of cores to use, default is detectCores() - 1.
#' @param filtercellNum, the number of non-zero expression estimate required to include the genes into the SCnorm fitting (default = 10). The initial grouping fits a quantile regression to each gene, making this value too low gives unstable fits.
#' @param ditherCount, FALSE of TRUE. Settin gto TRUE might improve results with UMI data
#' @param PropToUse, as default is set to 0.25 but to increase speed with large data set could be reduced, e.g. 0.1
#' @return a PDF providing a view of effects of normalization, a Rda file containing the full output of **SCnorm** and a tab delimited file containing the normalized data.
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#'     system("gzip -d singlecells_counts.txt.gz")
#'     conditions=rep(1,288)
#'     scnorm(group="docker", data.folder=getwd(),counts.matrix="singlecells_counts.txt",
#'     conditions=conditions,outputName="singlecells_counts", nCores=8, filtercellNum=10, ditherCount=TRUE, PropToUse=0.1)
#' }
#' @export
scnorm <- function(group=c("sudo","docker"), data.folder=getwd(), counts.matrix, conditions=NULL, outputName, nCores=8, filtercellNum = 10, ditherCount=FALSE, PropToUse=0.1){

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  setwd(data.folder)
  if(is.null(conditions)){
    cat("\nERROR: Conditions are missing\n")
  }else{
    conditions <- paste(conditions, collapse = "_")
  }
  if(group=="sudo"){
        params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder,":/data -d docker.io/repbioinfo/r340.2017.01 Rscript /bin/scnorm.R ",counts.matrix," ",conditions," ",outputName," ",nCores," ",filtercellNum, " ",ditherCount," ",PropToUse, sep="")
        runDocker(group="sudo",container="docker.io/repbioinfo/r340.2017.01", params=params)
  }else{
        params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder,":/data -d docker.io/repbioinfo/r340.2017.01 Rscript /bin/scnorm.R ",counts.matrix," ",conditions," ",outputName," ",nCores," ",filtercellNum," ",ditherCount," ",PropToUse, sep="")
        runDocker(group="docker",container="docker.io/repbioinfo/r340.2017.01", params=params)
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
    tmp.run[length(tmp.run)+1] <- paste("casc SCnorm user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc SCnorm system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc SCnorm elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("casc user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc SCnorm system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("casc SCnorm elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))

  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))

  system(paste("docker rm ", container.id, sep=""))

}
