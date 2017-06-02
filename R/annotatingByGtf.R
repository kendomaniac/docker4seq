#' @title Annotating RSEM gene.results using ENSEMBL gtf and refGenome CRAN package
#' @description This function executes the docker container annotate.1, where refGenome is used to annotated gene.results and isoforms.results outputs from RSEM using ENSEMBL GTF annotation
#' @param rsem.folder, a character string indicating where gene.results file is located
#' @param genome.folder, a character string indicating the folder for the genome reference used for mapping and counting with \code{"rsemstar"} function. In this folder is present the GTF used for by RSEM
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.
#' @import refGenome
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
rsemannoByGtf <- function(group=c("sudo", "docker"), rsem.folder=getwd(), genome.folder){

  if(group=="sudo"){
    params <- paste("--cidfile ",rsem.folder,"/dockerID -v ",rsem.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/r332.2017.01 Rscript /bin/.rsemannoByGtf.R", sep="")
    runDocker(group="sudo",container="docker.io/rcaloger/r332.2017.01", params=params)
  }else{
    params <- paste("--cidfile ",rsem.folder,"/dockerID -v ",rsem.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/rcaloger/r332.2017.01 Rscript /bin/.rsemannoByGtf.R", sep="")
    runDocker(group="docker",container="docker.io/rcaloger/r332.2017.01", params=params)
  }
  #saving log and removing docker container
  container.id <- readLines(paste(rsem.folder,"/dockerID", sep=""))
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
#  system(paste("docker rm ", container.id, sep=""))
  
}
