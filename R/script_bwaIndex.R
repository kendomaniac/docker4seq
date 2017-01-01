#' @title Generating bwa genome index
#' @description This function executes the docker container bwa1 where BWA is installed. The index is created using ENSEMBL genome fasta file. User needs to provide the URL for ENSEMBL genome located in the ENSEMBL ftp
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa will be located
#' @param ensembl.url, a character string indicating the URL from ENSEMBL ftp for the unmasked genome sequence of interest
#'
#' @return The indexed bwa genome reference sequence
#' @examples
#'\dontrun{
#'     #running bwa index
#'     bwaIndex(group="sudo",genome.folder="/sto2/data/scratch/hg38",
#'     ensembl.url="ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz")
#' }

bwaIndex <- function(group=c("sudo","docker"),genome.folder=getwd(), ensembl.url){
	cat("\nsetting as working dir the genome folder and running bwa docker container\n")

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/bwa.1")
		system(paste("sudo docker run -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.index.sh "," ",genome.folder," ",ensembl.url, sep=""))
	}else{
		system("docker pull docker.io/rcaloger/bwa.1")
		system(paste("docker run -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.1 sh /bin/bwa.index.sh "," ",genome.folder," ",ensembl.url, sep=""))
	}
}

