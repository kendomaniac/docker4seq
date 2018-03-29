#' @title Running small RNA-seq single-end reads alignment and quantification using BWA and custom scripts
#' @description This function executes the docker container where BWA is installed. BWA is a read alignment package that efficiently align short sequencing reads against a large reference sequence. Alignment is performed against annotations of human small RNAs. Read count is performed by GenomicAlignments R package and custom Python and bash commands.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where trimmed fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param ref.folder, a character string indicating the folder where the indexed reference genome for bwa is located
#' @param nh, a booleal value indicating if the analysis of non human small RNAs should be performed
#' @param fastq.id, a character string indicating the name of the fastq file to analyse
#' @param threads, a number indicating the number of cores to be used from the application
#' @param sample.id, a character string indicating the unique identifier of the dataset to analyse.
#' @param g.id, character string indicating the name human reference genome
#' @param refh.id, character string indicating the name human reference small RNA gene annotations
#' @param refnh.id, list of character strings indicating the name of references of non human small RNA annotations
#' @author Giulio Ferrero
#
#' @return read count files:
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     #running sncRNA quantification pipeline
#'     sncRNA(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     ref.folder="/data/ref", nh=TRUE, fastq.id=test_R1.fastq.gz,
#'     threads=24, sample.id="test", g.id=hg38.fa, refh.id = small_ncRNA.fa, refnh.id = c(virus.fa, bacteria.fa))
#'
#' }
#' @export

sncRNA <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder, ref.folder, nh=T, threads=1, fastq.id, sample.id, g.id, refh.id, refnh.id){

  home <- getwd()
  setwd(fastq.folder)

  #initialize status
  system("echo 0 >& ExitStatusFile")
  
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 >& ExitStatusFile")
    return(10)
  }

  ###################check scratch folder exist#################

  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    system("echo 3 >& ExitStatusFile")
    return(3)
  }

  ##############################################################

  tmp.folder <- gsub(":","-",gsub(" ","-",date()))

  cat("\nsetting as working dir the scratch folder and running docker container\n")
  cat("\nsetting as working dir the scratch folder and running sncRNA docker container\n")

  if(nh=="FALSE"){

    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", scratch.folder,":/data/scratch -v ", ref.folder,":/data/ref -v ", fastq.folder,":/data/input -d gferrero/sncrna sh /bin/sncRNA.sh ", tmp.folder, " ", threads, " ", fastq.id, " ", sample.id, " ", g.id, " ", refh.id, sep="")
    resultRun <- runDocker(group=group, container="gferrero/sncrna", params=params)

  }

  if(nh=="TRUE"){
    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", scratch.folder,":/data/scratch -v ", ref.folder,":/data/ref -v ", fastq.folder,":/data/input -d gferrero/sncrna sh /bin/sncRNA_nh.sh ", tmp.folder, " ", threads, " ", fastq.id, " ", sample.id, " ", g.id, " ", refh.id, sep="")

    for (i in 1:length(refnh.id)){
      params <- paste(params,paste(refnh.id[i], sep=""), sep=" ")
    }

    resultRun <- runDocker(group=group, container="gferrero/sncrna", params=params)
  }

  #    if(resultRun=="false"){
  #      system(paste("cp ", docker_fastq.folder, "/* ", fastq.folder, sep=""))
  #    }

  ##############################################################

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(fastq.folder)
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
  container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", "sncRNA_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  #removing temporary folder
  cat("\n\nRemoving the bwa temporary file ....\n")

  system(paste("rm  -f ",fastq.folder,"/dockerID", sep=""))
  system(paste("rm  -f ",fastq.folder,"/tempFolderID", sep=""))

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
  setwd(home)
}
