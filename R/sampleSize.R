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
#'    sampleSize(group="docker", filename="_counts.txt", power=0.80, FDR=0.1,
#'    genes4dispersion=200, log2fold.change=1)
#'}
#' @export

sampleSize <- function(group=c("sudo","docker"), filename, power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1, output.folder=getwd()){


  #running time 1
  ptm <- proc.time()
  
  home <- getwd()
  setwd(output.folder)
  
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    #initialize status
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }
  #removing the path from filename
  filename.tmp <- unlist(strsplit(filename,'/'))
  filename <-  filename.tmp[length(filename.tmp)]


  params <- paste("--cidfile ",output.folder, "/dockerID -v ",output.folder,":/data/scratch -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/.sampleSize.R ", filename, " ", power, " ", FDR, " ", genes4dispersion, " ", log2fold.change, sep="")
  resultRun <- runDocker(group=group, params=params)

  if(resultRun==0){
    cat("\nSample size analysis is finished\n")
  }
  


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
  #saving log and removing docker container
  container.id <- readLines(paste(output.folder,"/dockerID", sep=""), warn = FALSE)
  #  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker logs ", container.id, " >& ","sampleSize_",substr(container.id,1,12),".log", sep=""))
  # system(paste("docker rm ", container.id, sep=""))
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",output.folder, sep=""))


  setwd(home)
}

