#' @title Generating bowtie2 genome index
#' @description This function executes the docker container bowtie2 where bowtie2-2.2.9 is installed. The index is created using ENSEMBL genome fasta file. User needs to provide the URL for ENSEMBL genome located in the ENSEMBL ftp
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for Bowtie will be located
#' @param ensembl.urlgenome, a character string indicating the URL from ENSEMBL ftp for the unmasked genome sequence of interest
#' @param ensembl.urlgtf, a character string indicating the URL from ENSEMBL ftp for the GTF for genome of interest
#' @param threads, a number indicating the number of cores to be used from the application
#' @author Raffaele Calogero
#'
#' @return The indexed Bowtie genome reference sequence
#' @examples
#'\dontrun{
#'     #running rsemstar index for human
#'     bowtie2Index(group="docker",genome.folder="/data/scratch/hg38bowtie2",
#'     ensembl.urlgenome=
#'     "ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz",
#'     ensembl.urlgtf=
#'     "ftp://ftp.ensembl.org/pub/release-87/gtf/homo_sapiens/Homo_sapiens.GRCh38.87.gtf.gz",
#'     threads=8)
#'
#' }
#' @export
bowtie2Index <- function(group=c("sudo","docker"),  genome.folder=getwd(), ensembl.urlgenome=NULL, ensembl.urlgtf=NULL, threads=1){

  home <- getwd()

  #########check scratch folder exist###########
  if (!file.exists(genome.folder)){
    cat(paste("\nIt seems that the ",genome.folder, "folder does not exist, I create it\n"))
    dir.create(genome.folder)
  }
  #############################################

  setwd(genome.folder)

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


cat("\nsetting as working dir the genome folder and running bowtie2 docker container\n")


	params <- paste("--cidfile ",genome.folder,"/dockerID -v ",genome.folder,":/genome"," -d repbioinfo/bowtie2.2018.01:bowtie2-2.2.9-R3.4.4-Bioconductor3.6-Biostrings_2.46.0 sh /bin/bowtie2.index.sh ",ensembl.urlgenome," ",ensembl.urlgtf," ",threads, sep="")
	resultRun <- runDocker(group=group, params=params)

  if(resultRun==0){
    cat("\nBOWTIE2 index generation is finished\n")
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
  system(paste("docker logs ", container.id, " >& ", "rsemstarIndex_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  #running time 2
  system(paste("rm ",genome.folder,"/dockerID", sep=""))
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",genome.folder, sep=""))
  setwd(home)

}

