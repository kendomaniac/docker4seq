#' @title Annotating RSEM gene.results using ENSEMBL gtf and refGenome CRAN package
#' @description This function executes the docker container annotate.1, where refGenome is used to annotated gene.results and isoforms.results outputs from RSEM using ENSEMBL GTF annotation
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param rsem.folder, a character string indicating where gene.results file is located
#' @param genome.folder, a character string indicating the folder for the genome reference used for mapping and counting with \code{"rsemstar"} function. In this folder is present the GTF used for by RSEM
#' @author Raffaele Calogero

#' @return one file: annotated_genes.results, which is the annotated version of gene.results.

#' @import utils
#' @examples
#' \dontrun{
##'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     library(docker4seq)
#'    #running rsemstar nostrand pe
#'    rsemstar(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch/",
#'          genome.folder="/data/genomes/hg38star/", seq.type="pe", strandness="none",
#'          threads=8, save.bam = FALSE)
#'     #running rsemannoByGtf
#'     rsemannoByGtf(group="docker", rsem.folder=getwd(), genome.folder="/data/scratch/hg38star")
#' }
#'
#' @export
rsemannoByGtf <- function(group="docker", rsem.folder=getwd(), genome.folder){

  #remembering actual folder
  home <- getwd()
  #setting rsem output folder as working dir
  setwd(rsem.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }


  params <- paste("--cidfile ",rsem.folder,"/dockerID -v ",rsem.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/.rsemannoByGtf.R", sep="")
    resultRun <- runDocker(group=group, params=params)


  if(resultRun==0){
    cat("\nGTF based annotation is finished is finished\n")
  }
  
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
