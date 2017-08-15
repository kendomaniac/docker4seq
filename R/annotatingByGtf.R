#' @title Annotating RSEM gene.results using ENSEMBL gtf and refGenome CRAN package
#' @description This function executes the docker container annotate.1, where refGenome is used to annotated gene.results and isoforms.results outputs from RSEM using ENSEMBL GTF annotation
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param rsem.folder, a character string indicating where gene.results file is located
#' @param genome.folder, a character string indicating the folder for the genome reference used for mapping and counting with \code{"rsemstar"} function. In this folder is present the GTF used for by RSEM
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.

#' @import utils
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/genes.results.gz")
#'     system("gzip -d genes.results.gz")
#'     #running rsemannoByGtf
#'     rsemannoByGtf(group="docker", rsem.folder=getwd(), genome.folder="/data/scratch/hg38star")
#' }
#'
#' @export
rsemannoByGtf <- function(group="docker", rsem.folder=getwd(), genome.folder){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

  if(group=="sudo"){
    params <- paste("--cidfile ",rsem.folder,"/dockerID -v ",rsem.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/r332.2017.01 Rscript /bin/.rsemannoByGtf.R", sep="")
    runDocker(group="sudo",container="docker.io/rcaloger/r332.2017.01", params=params)
  }else{
    params <- paste("--cidfile ",rsem.folder,"/dockerID -v ",rsem.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/r332.2017.01 Rscript /bin/.rsemannoByGtf.R", sep="")
    runDocker(group="docker",container="docker.io/rcaloger/r332.2017.01", params=params)
  }

  out <- "xxxx"
  #waiting for the end of the container work
  while(out != "anno.info"){
    Sys.sleep(10)
    cat(".")
    out.tmp <- dir(file.path(rsem.folder))
    out.tmp <- out.tmp[grep("anno.info",out.tmp)]
    if(length(out.tmp)>0){
      out <- "anno.info"
    }
  }


  #remembering actual folder
  home <- getwd()
  #setting rsem output folder as working dir
  setwd(rsem.folder)

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(rsem.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
  con <- file("run.info", "r")
  tmp.run <- readLines(con)
  close(con)
    tmp.run[length(tmp.run)+1] <- paste("rsemannoByGtf user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("rsemannoByGtf system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("rsemannoByGtf elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("rsemannoByGtf user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("rsemannoByGtf system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("rsemannoByGtf elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(rsem.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",rsem.folder, sep=""))
  setwd(home)
}
