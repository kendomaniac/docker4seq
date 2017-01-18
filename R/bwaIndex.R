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
#'     ensembl.url=
#'     "ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz")
#' }
#' @export
bwaIndex <- function(group=c("sudo","docker"),genome.folder=getwd(), ensembl.url){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

	cat("\nsetting as working dir the genome folder and running bwa docker container\n")

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/bwa.2017.01")
		system(paste("sudo docker run --privileged=true -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.2017.01 sh /bin/bwa.index.sh "," ",genome.folder," ",ensembl.url, sep=""))
	}else{
		system("docker pull docker.io/rcaloger/bwa.2017.01")
		system(paste("docker run --privileged=true -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.2017.01 sh /bin/bwa.index.sh "," ",genome.folder," ",ensembl.url, sep=""))
	}
	out <- "xxxx"
	#waiting for the end of the container work
	while(out != "out.info"){
	  Sys.sleep(10)
	  cat(".")
	  out.tmp <- dir(genome.folder)
	  out.tmp <- out.tmp[grep("out.info",out.tmp)]
	  if(length(out.tmp)>0){
	    out <- "out.info"
	  }
	}
	#running time 2
	ptm <- proc.time() - ptm
	con <- file(paste(genome.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)

	tmp.run <- NULL
	tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run, paste(genome.folder,"run.info", sep="/"))
  #running time 2
}

