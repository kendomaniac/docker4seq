#' @title Preparing the bed file required by starchip to detect circular RNAs on paired-end sequences
#' @description This function prepare the genome bed file for starchip, the GTF must be the one used by starChimeric
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return the bed files of the reference genome for STARChip analysis
#' @examples
#'\dontrun{
#'     starChipIndex(group="docker", genome.folder="/data/genomes/hg38star")
#' }
#' @export

starChipIndex <- function(group=c("sudo","docker"), genome.folder=getwd()){

  home <- getwd()
  genome.folder <- normalizePath(genome.folder)
  setwd(genome.folder)
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

  if(group=="sudo"){
    params <- paste("--cidfile ", genome.folder,"/dockerID -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starchipIndex.sh", sep="")
    resultRun <- runDocker(group="sudo", params=params)
  }else{
    params <- paste("--cidfile ", genome.folder,"/dockerID -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starchipIndex.sh", sep="")
    resultRun <- runDocker(group="docker", params=params)
  }

  if(resultRun=="false"){
    cat("\nThe STARChip bed creation is finished\n")
  }


  #running time 2
  ptm <- proc.time() - ptm
  con <- file(paste(genome.folder,"run.info", sep="/"), "r")
  tmp.run <- readLines(con)
  close(con)
  tmp.run[length(tmp.run)+1] <- paste("STARChip index user run time mins ",ptm[1]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("STARChip index system run time mins ",ptm[2]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("STARChip index elapsed run time mins ",ptm[3]/60, sep="")
  writeLines(tmp.run,paste(genome.folder,"run.info", sep="/"))
  #running time 2
  #removing temporary folder
  #saving log and removing docker container
  container.id <- readLines(paste(genome.folder,"/dockerID", sep=""), warn = FALSE)
  #    system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker logs ", container.id, " >& ","starChipIndex_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))


  cat("\n\nRemoving the starChipIndex temporary file ....\n")
  system(paste("rm  -f ",genome.folder,"/dockerID", sep=""))
  setwd(home)

}
