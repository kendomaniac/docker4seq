#' @title A wrapper function for experiment_power from RnaSeqSampleSize Bioconductor package
#' @description This function evaluate the statistical power of a pilot experiment
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param filename, a character string indicating the name of the count table file
#' @param replicatesXgroup, an integer indicating the number of samples used in each group
#' @param FDR, false discovery rate
#' @param genes4dispersion, an integer indicating the number of genes used in estimation of read counts and dispersion distribution
#' @param log2fold.change, an integer indicating the minimum log2 fold change for prognostic genes between two groups
#' @param output.folder, a string indicating the path where to save the output file
#' @author Raffaele Calogero
#' 
#' @return a string with the requested informations. The string is also saved in a file: power_evaluation.txt
#' @examples
#'\dontrun{
#'  system("wget 130.192.119.59/public/test.analysis.zip")
#'  unzip("test.analysis.zip")
#'  setwd("test.analysis")
#'  library(docker4seq)
#'  experimentPower(group="docker",filename="_counts.txt",replicatesXgroup=7,
#'  FDR=0.1, genes4dispersion=200, log2fold.change=1)
#'}
#' @export


experimentPower <- function(group=c("sudo","docker"), filename, replicatesXgroup=3, FDR=0.1, genes4dispersion=200, log2fold.change=1,  output.folder=getwd()){


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
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }
  #removing the path from filename
  filename.tmp <- unlist(strsplit(filename,'/'))
  filename <-  filename.tmp[length(filename.tmp)]

  params <- paste("--cidfile ",output.folder, "/dockerID -v ",output.folder,":/data/scratch -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/.experimentPower.R ", filename, " ", replicatesXgroup, " ", FDR, " ", genes4dispersion, " ", log2fold.change, sep="")
  resultRun <- runDocker(group=group, params=params)


  if(resultRun==0){
    cat("\nExperiment power analysis is finished\n")
  }
  



  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(output.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("experimentPower user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("experimentPower system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("experimentPower elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("experimentPower user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("experimentPower system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("experimentPower elapsed run time mins ",ptm[3]/60, sep="")
  }
    writeLines(tmp.run,"run.info")
    system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",output.folder, sep=""))

    #saving log and removing docker container
    container.id <- readLines(paste(output.folder,"/dockerID", sep=""), warn = FALSE)
    system(paste("docker logs ", container.id, " >& ", "expermentPower_",substr(container.id,1,12),".log", sep=""))
    system(paste("docker rm ", container.id, sep=""))

    system("rm -fR anno.info")
    system("rm -fR dockerID")

    setwd(home)
}
