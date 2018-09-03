#' @title Running small RNA-seq single-end reads alignment and quantification using BWA and custom scripts
#' @description This function executes the docker container where BWA is installed. BWA is a read alignment package that efficiently align short sequencing reads against a large reference sequence. Alignment is performed against annotations of human small RNAs. Read count is performed by GenomicAlignments R package and custom Python and bash commands.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where trimmed fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param mode, a character string indicating the required type of analysis. Compatible analyses mode are "miRNA" and "ncRNA". In "miRNA" analysis mode, the version ("mb.version" argument) and species prefix ("mb.species" argument) of miRBase are required. This mode require also the "reference" argument. In the "ncRNA" mode only the "reference" argument is required.
#' @param reference, a character string indicating the path to the reference fasta file used to create the BWA index
#' @param threads, a number indicating the number of cores to be used from the application
#' @param mb.version, a character string indicating the required version of miRBase database. Visit ftp://mirbase.org/pub/mirbase/ to select the proper version id.
#' @param mb.species, a character string indicating the three-letter prefix of a species annotated in miRBase (e.g. "hsa" for human miRNAs). Please refer to http://www.mirbase.org/help/genome_summary.shtml to obtain the proper species prefix.
#' 
#' @author Giulio Ferrero
#
#' @return read count files:
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'
#'     #running miRNAs quantification pipeline
#'     bwaIndex(group="docker", genome.folder="/data/genomes", mb.version="22", mb.species="hsa", mode="miRNA")
#'     sncRNA(group="docker", fastq.folder=getwd(), scratch.folder="/data/scratch", mode="miRNA", reference="/data/genome/hairpin_hsa_miRBase_22.fa", threads=8, mb.version="22", mb.species="hsa")
#'     
#'     #running non miRNA ncRNAs quantification pipeline
#'     bwaIndex(group="docker", genome.folder="/data/genomes/", rc.version="9.0", rc.species="Homo sapiens", length=80, mode="ncRNA")
#'     sncRNA(group="docker", fastq.folder=getwd(), scratch.folder="/data/scratch", mode="ncRNA", reference="/data/genome/ncRNA_Homo_sapiens_RNA_Central_9.0_len_80.fa", threads=8)
#'
#' }
#' @export

sncRNA <- function(group=c("sudo","docker"),fastq.folder=getwd(), scratch.folder, mode, reference, threads=1, mb.version=NULL, mb.species=NULL){

  home <- getwd()
  setwd(fastq.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    return(10)
  }

  ###################check scratch folder exist#################

  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    return(3)
  }

  ##############################################################

  tmp.folder <- gsub(":","-",gsub(" ","-",date()))

  cat("\nsetting as working dir the scratch folder and running docker container\n")
  cat("\nsetting as working dir the scratch folder and running sncRNA docker container\n")

ref.folder=dirname(reference)
ref.id=basename(reference)
  
  if(mode=="miRNA"){
    
    mb_ok_ver = c("1.0","10.0","10.1","1.1","11.0","1.2","12.0","1.3","13.0","14","1.4","15","1.5","16","17","18","19","20","2.0","21","2.1","22","2.2","3.0","3.1","4.0","5.0","5.1","6.0","7.0","7.1","8.0","8.1","8.2","9.0","9.1","9.2")
    
    if(mb.version %in% mb_ok_ver == FALSE){
      cat("\nThe miRBase version is not correct\n")
      system("echo 2 > ExitStatusFile 2>&1")
      setwd(home)
      return(2)
    }
    
    if(is.null(mb.species)){
      cat("\nPlease insert a proper miRBase species identifier\n")
      system("echo 2 > ExitStatusFile 2>&1")
      setwd(home)
      return(2)
    }
    
    else{
    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", scratch.folder,":/data/scratch -v ", ref.folder,":/data/ref -v ", fastq.folder,":/data/input -d docker.io/gferrero/sncrna /bin/bash /bin/sncRNA.sh ", mode, " ", threads, " ", ref.id, " ", mb.version, " ", mb.species, sep="")
    }
  }

  if(mode=="ncRNA"){
    
    params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", scratch.folder,":/data/scratch -v ", ref.folder,":/data/ref -v ", fastq.folder,":/data/input -d docker.io/gferrero/sncrna /bin/bash /bin/sncRNA.sh ", mode, " ", threads, " ", ref.id, sep="")
    
  }

resultRun <- runDocker(group=group, params=params)
  
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
