#' @title A function to create a Salmon pseudo reference
#' @description This function executes the Salmon docker that produces as output a transcripts index file.
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param index.folder, a character string indicating the folder where transcriptime index will be created.
#' @param ensembl.ensembl.urltranscriptome, a character string indicating the URL from ENSEMBL ftp for the transcripts fasta file of interest
#' @param ensembl.urlgtf, a character string indicating the URL from ENSEMBL ftp for the GTF for genome of interest
#' @param k, a number indicating the k-mers length, 31 eems to work well for reads of 75bp or longer, but you might consider a smaller k if dealing with shorter reads.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit University of Torino Italy
#' 
#' @examples
#' \dontrun{
#'     #running skeleton
#'     salmonIndex(group="docker", index.folder=getwd(), 
#'     ensembl.urltranscriptome="ftp://ftp.ensembl.org/pub/release-90/fasta/mus_musculus/cdna/Mus_musculus.GRCm38.cdna.all.fa.gz",
#'     ensembl.urlgtf="ftp://ftp.ensembl.org/pub/release-90/gtf/mus_musculus/Mus_musculus.GRCm38.90.gtf.gz", 
#'     k=31)
#' }
#'
#' @export
salmonIndex <- function(group=c("sudo","docker"), index.folder, ensembl.urltranscriptome, ensembl.urlgtf, k=31){

  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #storing the position of the home folder  
  home <- getwd()
  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(index.folder)){
    cat(paste("\nIt seems that the ",index.folder, " folder does not exist\n"))
    return(2)
  }
  setwd(index.folder)
  system(paste("wget -O transcripts.fa.gz ", ensembl.urltranscriptome, sep=""))
  system("gzip -d transcripts.fa.gz")
  system(paste("wget -O genome.gtf.gz ",ensembl.urlgtf, sep=""))
  system("gzip -d genome.gtf.gz")
  #executing the docker job
  if(group=="sudo"){
#    params <- paste("--cidfile ",index.folder,"/dockerID -v ", index.folder, ":/index -d docker.io/repbioinfo/salmon.2017.01 /usr/local/bin/salmon index -t /index/transcripts.fa -i /index/transcripts_index --type quasi -k ",k, sep="")
    resultRun <- runDocker(group="sudo",container="docker.io/repbioinfo/salmon.2017.01", params=params)
  }else{
#    params <- paste("--cidfile ",index.folder,"/dockerID -v ", index.folder, ":/index -d docker.io/repbioinfo/salmon.2017.01 /usr/local/bin/salmon index -t /index/transcripts.fa -i /index/transcripts_index --type quasi -k ",k, sep="")
     params <- paste("--cidfile ",index.folder,"/dockerID -v ", index.folder, ":/index -d docker.io/repbioinfo/salmon.2017.01 sh /bin/salmon_index.sh ",k, sep="")
     resultRun <- runDocker(group="docker",container="docker.io/repbioinfo/salmon.2017.01", params=params)
  }
  #waiting for the end of the container work
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(index.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(index.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",index.folder,"/", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  if(resultRun=="false"){
     system("rm -fR dockerID")
     system("rm  -fR tempFolderID")
     system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  }else{
    cat(paste("there was an error in the execution of ", substr(container.id,1,12), "\n"), sep="")
  }
  setwd(home)
}
