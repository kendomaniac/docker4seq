#' @title Generating rsem-star genome index
#' @description This function executes the docker container rsem-star1 where RSEM and STAR are installed. The index is created using ENSEMBL genome fasta file. User needs to provide the URL for ENSEMBL genome located in the ENSEMBL ftp
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa will be located
#' @param ensembl.urlgenome, a character string indicating the URL from ENSEMBL ftp for the unmasked genome sequence of interest
#' @param ensembl.urlgtf, a character string indicating the URL from ENSEMBL ftp for the GTF for genome of interest
#' @param threads, a number indicating the number of cores to be used from the application
#'
#' @return The indexed bwa genome reference sequence
#' @examples
#'\dontrun{
#'     #running bwa index
#'     rsemstarIndex(group="sudo",genome.folder="/data/scratch/hg38star",
#'     ensembl.urlgenome=
#'     "ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz",
#'     ensembl.urlgtf=
#'     "ftp://ftp.ensembl.org/pub/release-87/gtf/homo_sapiens/Homo_sapiens.GRCh38.87.gtf.gz",
#'     threads=24)
#' }
#' @export
rsemstarIndex <- function(group=c("sudo","docker"),genome.folder=getwd(), ensembl.urlgenome, ensembl.urlgtf, threads=1){
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

  cat("\nsetting as working dir the genome folder and running bwa docker container\n")

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/rsemstar1")
		system(paste("sudo docker run -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar1 sh /bin/rsemstar.index.sh "," ",genome.folder," ",ensembl.urlgenome," ",ensembl.urlgtf," ",threads, sep=""))
	}else{
		system("docker pull docker.io/rcaloger/rsemstar1")
		system(paste("docker run -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/rsemstar1 sh /bin/rsemstar.index.sh "," ",genome.folder," ",ensembl.urlgenome," ",ensembl.urlgtf," ",threads, sep=""))
	}
}

