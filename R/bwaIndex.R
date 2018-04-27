#' @title Generating bwa genome index
#' @description This function executes the docker container bwa1 where BWA is installed. Optionally, the index can be created also for GATK bundle data genome fasta file.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa will be located
#' @param genome.fasta, a character string indicating the local path to the genome fasta file to index
#' @param genome.url, a character string indicating the URL from download web page for the genome sequence of interest
#' @param dbsnp.file, a character string indicating the name of dbSNP vcf located in the genome folder. The dbSNP vcf, dbsnp_138.b37.vcf.gz and dbsnp_138.hg19.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37
#' @param g1000.file, a character string indicating the name of 1000 genome vcf located in the genome folder. The 1000 genomes vcf, Mills_and_1000G_gold_standard.indels.b37.vcf.gz and Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37/
#' @param gatk, a boolean TRUE and FALSE that indicate if the index will be used for GATK analysis
#' @param download.genome, a boolean TRUE and FALSE that indicate if the genome file must but be downloaded
#' @author Giulio Ferrero
#'
#' @return The indexed bwa genome reference sequence
#' @examples
#'\dontrun{
#'
#'     #running bwa index
#'     bwaIndex(group="sudo",genome.folder="/data/genomes/mm10bwa", genome.fasta,
#'     gatk=FALSE, download.genome=FALSE)
#
#'     #running bwa index for gatk
#'     bwaIndex(group="sudo",genome.folder="/data/genomes/hg19_bwa", genome.url=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz",
#'     dbsnp.file="dbsnp_138.hg19.vcf.gz", g1000.file="Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.gz",
#'     gatk=TRUE, download.genome=TRUE)
#'
#'
#' }
#' @export
bwaIndex <- function(group=c("sudo","docker"), genome.folder=getwd(), genome.fasta, genome.url=NULL, dbsnp.file=NULL, g1000.file=NULL, gatk=FALSE, download.genome=FALSE){

  #########check genome folder exist###########
  if (!file.exists(genome.folder)){
    cat(paste("\n",genome.folder, "folder does not exist, It will be created \n"))
    if (!dir.create(genome.folder)){
      cat(paste("\nError ",genome.folder, "folder cannot be created\n"))
      system("echo 4 >& ExitStatusFile")
      return(4) 
    }
  }
  #############################################
  
  home <- getwd()
  setwd(genome.folder)
  #initialize status
  system("echo 0 >& ExitStatusFile")
  
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    #initialize status
    system("echo 10 >& ExitStatusFile")
    setwd(home)
    return(10)
  }


  if (!file.exists(genome.fasta)){
    cat("\nERROR:",genome.fasta, "does not exist\n")
    #initialize status
    system("echo 1 >& ExitStatusFile")
    setwd(home)
    return(1)
    }
  
  filename.genome.fasta=basename(genome.fasta)
  path.genome.fasta=dirname(genome.fasta)

	cat("\nSetting as working dir the genome folder and running bwa docker container\n")

  if(gatk){
    if(length(dir[grep(sub(".vcf.gz$", "", dbsnp.file),dir)])<2){
      cat("\ndbSNP vcf.gz and/or vcf.idx.gz missing\n")
      system("echo 2 >& ExitStatusFile")
      setwd(home)
      return(2)
    }else{
      cat("\nPreparing dbsnp vcf\n")
      system(paste("gzip -d ", dbsnp.file, sep=""))
      system(paste("gzip -d ", sub(".vcf.gz$", ".vcf.idx.gz$",dbsnp.file), sep=""))
      system(paste("mv ", sub(".gz$", "",dbsnp.file), " dbsnp.vcf", sep=""))
    }
    if(length(dir[grep(sub(".vcf.gz$", "", g1000.file),dir)])<2){
      cat("\1000 genomes vcf and/or vcf.idx.gz missing\n")
      system("echo 3 >& ExitStatusFile")
      setwd(home)
      return(3)
    }else{
      cat("\nPreparing 1000 genomes vcf\n")
      system(paste("gzip -d ", g1000.file, sep=""))
      system(paste("gzip -d ", sub(".vcf.gz$", ".vcf.idx.gz$",g1000.file), sep=""))
      system(paste("mv ", sub(".gz$", "",g1000.file), " g1k.vcf", sep=""))
    }
  }

  params <- paste("--cidfile ",genome.folder,"/dockerID -v ",genome.folder,":/data/scratch",path.genome.fasta,":/data/ref"," -d docker.io/repbioinfo/bwa.2017.01 sh /bin/bwa.index.sh "," ",genome.folder, " ", gatk, " ", genome.url, download.genome, filename.genome.fasta, sep="")
  
	  resultRun <- runDocker(group=group, params=params)
  if(resultRun==0){
    cat("\nBwa index generation is finished\n")
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
	system(paste("rm ",genome.folder,"/out.info",sep=""))
	
	#saving log and removing docker container
	container.id <- readLines(paste(genome.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))
	
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",genome.folder, sep=""))
  setwd(home)
}

