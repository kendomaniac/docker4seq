#' @title Annotating RSEM gene.results using ENSEMBL gtf and refGenome CRAN package
#' @description This function executes the docker container annotate.1, where refGenome is used to annotated gene.results and isoforms.results outputs from RSEM using ENSEMBL GTF annotation
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param peaks.file, a character string indicating the MACS peak file, extension _peaks.xls, with full path  
#' @param gtf.file, a character string indicating the file, with full path for the genome gtf 
#' @param extension, a number defining how many nucleotides should be expandend the extremes of the targetr gene to find an overlap with peaks, default 10000
#' @author Raffaele Calogero

#' @return one file: MACS2 peaks annotated

#' @import utils
#' @examples
#' \dontrun{
##'     #downloading fastq files

#'     #running rsemannoByGtfchipseq
#'     annoByGtfchipseq(group="docker", peaks.file=paste(getwd(),"h3k9me1_igg_peaks.xls", sep="/"), 
#'                gtf.file=paste("/Users/raffaelecalogero/Dropbox/courses/DUKENUS_JUL2019/course/course/datasets/genomes/mm10bwa","Mus_musculus.GRCm38.97.gtf", sep="/"), extension=10000)
#' }
#'
#' @export
annoByGtfchipseq <- function(group="docker", peaks.file=getwd(), gtf.file, extension=10000){

  #remembering actual folder
  home <- getwd()
  #setting rsem output folder as working dir
  data.folder <- dirname(peaks.file)
  setwd(data.folder)
  
  genome.folder <- dirname(gtf.file)
  
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


  params <- paste("--cidfile ", data.folder,"/dockerID -v ",data.folder,":/data/scratch -v ",genome.folder,":/data/genome -d docker.io/repbioinfo/r332.2017.01 Rscript /bin/annoByGtfchipseq.R ", basename(gtf.file), " ", basename(peaks.file), " ", extension, sep="")
  resultRun <- runDocker(group=group, params=params)


  if(resultRun==0){
    cat("\nGTF based annotation is finished \n")
  }
  
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
  con <- file("run.info", "r")
  tmp.run <- readLines(con)
  close(con)
    tmp.run[length(tmp.run)+1] <- paste("annoByGtfchipseq user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("annoByGtfchipseq system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("annoByGtfchipseq elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("annoByGtfchipseq user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("annoByGtfchipseq system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("annoByGtfchipseq elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),"_annoByGtfchipseq.log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  system("rm -fR anno.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))

  setwd(home)
}
