#' @title Generating rsem-star genome index
#' @description This function executes the docker container rsem-star1 where RSEM and STAR are installed. The index is created using ENSEMBL genome fasta file. User needs to provide the URL for ENSEMBL genome located in the ENSEMBL ftp
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome will be located
#' @param uscs.urlgenome, a character string indicating the URL from uscs download web page for the unmasked genome sequence of interest
#' @param uscs.gtf, a character string indicating the path of the GTF file for genome of interest
#' @param uscs.urlknownIsoforms, a character string indicating the URL from uscs download web page for the knowisoforms file for genome of interest
#' @param uscs.urlknownToLocusLink, a character string indicating the URL from uscs download web page for the knownToLocusLink file for genome of interest
#' @param threads, a number indicating the number of cores to be used from the application
#' @author Raffaele Calogero
#'
#' @return The indexed genome reference sequence
#' @examples
#'\dontrun{
#'     #running rsemstar index for human
#'     rsemstarUscsIndex(group="sudo",genome.folder="/data/scratch/hg19UCSCstar",
#'     uscs.urlgenome=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz",
#'     uscs.gtf=
#'     "/Users/raffaelecalogero/Desktop/hg19_ucsc.gtf.gz",
#'     uscs.urlknownIsoforms=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/knownIsoforms.txt.gz",
#'     uscs.urlknownToLocusLink=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/knownToLocusLink.txt.gz",
#'     threads=24)
#'
#' }
#' @export
rsemstarUscsIndex <- function(group=c("sudo","docker"), genome.folder=getwd(),
                          uscs.urlgenome=NULL, uscs.gtf=NULL,
                          uscs.urlknownIsoforms=NULL, uscs.urlknownToLocusLink=NULL,
                          threads=1){

  home <- getwd()
  setwd(genome.folder)
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #########check scratch folder exist###########
  if (!file.exists(genome.folder)){
    cat(paste("\nIt seems that the ",genome.folder, "folder does not exist, I create it\n"))
    dir.create(genome.folder)
  }
  #############################################
  cat("\nsetting as working dir the genome folder and running star docker container\n")

  cat("\ncopying the gtf in the genome folder\n")
  system(paste("cp ", uscs.gtf," ",genome.folder,"/genome.gtf.gz", sep=""))
  system(paste("gzip -d ",genome.folder,"/genome.gtf.gz", sep=""))

	if(group=="sudo"){
	     params <- paste("--cidfile ",genome.folder,"/dockerID -v ",genome.folder,":/data/scratch"," -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsemstarUCSC.index.sh "," ",genome.folder," ",uscs.urlgenome," ",uscs.gtf," ",threads," ",uscs.urlknownIsoforms," ", uscs.urlknownToLocusLink, sep="")
	     resultRun <- runDocker(group="sudo", params=params)
	  }else{
#		system("docker pull docker.io/repbioinfo/rsemstar.2017.01")
	    params <- paste("--cidfile ",genome.folder,"/dockerID -v ",genome.folder,":/data/scratch"," -d docker.io/repbioinfo/rsemstar.2017.01 sh /bin/rsemstarUCSC.index.sh "," ",genome.folder," ",uscs.urlgenome," ",uscs.gtf," ",threads," ",uscs.urlknownIsoforms," ", uscs.urlknownToLocusLink, sep="")
	    resultRun <- runDocker(group="docker", params=params)
	  }

    if(resultRun=="false"){
    cat("\nrsemstarUscsIndex run is finished\n")
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

  #saving log and removing docker container
  container.id <- readLines(paste(genome.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", "rsemstarUscsIndex_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))


  #running time 2
  system(paste("rm ",genome.folder,"/dockerID", sep=""))
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",genome.folder, sep=""))
  setwd(home)

}

