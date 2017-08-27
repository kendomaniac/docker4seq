#' @title A wrapper function for sample_size_distribution function from RnaSeqSampleSize Bioconductor package
#' @description This function executes sample_size_distribution to identify the number of samples x group needed to obtain a specific statistical power
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param filename, a character string indicating the name of the count table file
#' @param power, expected statistical power required to detect prognostic genes
#' @param FDR, false discovery rate
#' @param genes4dispersion, an integer indicating the number of genes used in estimation of read counts and dispersion distribution
#' @param log2fold.change, an integer indicating the minimum log2 fold change for prognostic genes between two groups
#' @param output.folder, a string indicating the path where to save the output file
#' @author Raffaele Calogero
#' 
#' @return a string with the requested informations. the string is also saved in a file: sample_size_evaluation.txt , power_evaluation.txt
#' @examples
#'\dontrun{
#'    system("wget 130.192.119.59/public/test.analysis.zip")
#'    unzip("test.analysis.zip")
#'    setwd("test.analysis")
#'    library(docker4seq)
#'    sampleSize("_counts.txt", power=0.80, FDR=0.1,
#'    genes4dispersion=200, log2fold.change=1)
#'}
#' @export

sampleSize <- function(group=c("sudo","docker"), filename, power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1, output.folder=getwd()){


  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #removing the path from filename
  filename.tmp <- unlist(strsplit(filename,'/'))
  filename <-  filename.tmp[length(filename.tmp)]

  if(group=="sudo"){
    params <- paste("--cidfile ",output.folder, "/dockerID -v ",output.folder,":/data/scratch -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/.sampleSize.R ", filename, " ", power, " ", FDR, " ", genes4dispersion, " ", log2fold.change, sep="")
    runDocker(group="sudo",container="docker.io/repbioinfo/r332.2017.01", params=params)
  }else{
    params <- paste("--cidfile ",output.folder, "/dockerID -v ",output.folder,":/data/scratch -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/.sampleSize.R ", filename, " ", power, " ", FDR, " ", genes4dispersion, " ", log2fold.change, sep="")
    runDocker(group="docker",container="docker.io/repbioinfo/r332.2017.01", params=params)
  }

  out <- "xxxx"
  #waiting for the end of the container work
  while(out != "anno.info"){
    Sys.sleep(10)
    cat(".")
    out.tmp <- dir(file.path(output.folder))
    out.tmp <- out.tmp[grep("anno.info",out.tmp)]
    if(length(out.tmp)>0){
      out <- "anno.info"
    }
  }

  home <- getwd()
  setwd(output.folder)

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(output.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("sampleSize user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sampleSize system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sampleSize elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("sampleSize user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sampleSize system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sampleSize elapsed run time mins ",ptm[3]/60, sep="")
  }
  writeLines(tmp.run,"run.info")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",output.folder, sep=""))
  system("rm -fR anno.info")
  system("rm -fR dockerID")

  setwd(home)
}

