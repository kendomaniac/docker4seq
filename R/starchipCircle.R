#' @title Running starchip to detect circular RNAs on paired-end sequences
#' @description This function execute starchip on a set of folders containing the output of starChimeric. It requires a specific bed generated with starChipIndex in teh genome folder used by starChimeric
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param samples.folder, the folder where are located all the folders of the samples processed with starChimeric
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running star2step nostrand pe
#'     starchipCircle(group="docker", genome.folder="/data/genomes/hg38star", samples.folder=getwd())
#' }
#' @export
starchipCircle <- function(group=c("sudo","docker"), genome.folder, samples.folder){

  home <- getwd()
  setwd(samples.folder)
  dir <- list.dirs(recursive = FALSE)
  dir <- sub("\\./","/samples/", dir)
  writeLines(dir, "STARdirs.txt")

  params.file=paste(path.package(package="docker4seq"),"extras/starchip-circles.params",sep="/")
  system(paste("cp ",params.file," ", samples.folder, "/Parameters.txt",sep=""))


  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

  if(group=="sudo"){
    params <- paste("--cidfile ", samples.folder,"/dockerID -v ", samples.folder,":/samples -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starChipCircle.sh", sep="")
    resultRun <- runDocker(group="sudo",container="docker.io/repbioinfo/star251.2017.01", params=params)
  }else{
    params <- paste("--cidfile ", samples.folder,"/dockerID -v ", samples.folder,":/samples -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starChipCircle.sh", sep="")
    resultRun <- runDocker(group="docker",container="docker.io/repbioinfo/star251.2017.01", params=params)
  }

  if(resultRun=="false"){
    cat("\nstarchipCircle runs are finished\n")
  }



  #running time 2
  ptm <- proc.time() - ptm
  con <- file(paste(fastq.folder,"run.info", sep="/"), "r")
  tmp.run <- readLines(con)
  close(con)
  tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
  writeLines(tmp.run,paste(fastq.folder,"run.info", sep="/"))
  #running time 2
  #removing temporary folder
  #saving log and removing docker container
  container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
  #    system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker logs ", container.id, " >& ","starchipCircle_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))


  cat("\n\nRemoving the starChipIndex temporary file ....\n")
  system(paste("rm  -f ",fastq.folder,"/dockerID", sep=""))
  setwd(home)

}
