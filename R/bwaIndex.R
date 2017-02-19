#' @title Generating bwa genome index for GATK variant call
#' @description This function executes the docker container bwa1 where BWA is installed. The index is created using GATK bundle data genome fasta file. User needs to dowload the file in the genome folder from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa will be located
#' @param dbsnp.file, a character string indicating the name of dbSNP vcf located in the genome folder. The dbSNP vcf, dbsnp_138.b37.vcf.gz and dbsnp_138.hg19.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37
#' @param g1000.file, a character string indicating the name of 1000 genome vcf located in the genome folder. The 1000 genomes vcf, Mills_and_1000G_gold_standard.indels.b37.vcf.gz and Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37/
#' @param fasta.file, a character string indicating the name of fasta file located in the genome folder. This fasta, ucsc.hg19.fasta.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/hg19/
#' @param gatk, a boolean TRUE and FALSE that indicate if the index will be used for GATK analysis
#'
#' @return The indexed bwa genome reference sequence
#' @examples
#'\dontrun{
#'     #running bwa index
#'     bwaIndex(group="sudo",genome.folder="/sto2/data/scratch/hg19_bwa", fasta.file="ucsc.hg19.fasta.gz" ,
#'     dbsnp.file="dbsnp_138.hg19.vcf.gz", g1000.file="Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.gz",
#'     gatk=FALSE)
#' }
#' @export
bwaIndex <- function(group=c("sudo","docker"),genome.folder=getwd(), fasta.file, dbsnp.file, g1000.file, gatk=FALSE){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

	cat("\nsetting as working dir the genome folder and running bwa docker container\n")
  dir <- dir(genome.folder)
  if(dir[grep(fasta.file,dir)]!=fasta.file){
    cat("\nfasta file is missing\n")
    return(1)
  }else{
    cat("\nPreparing genome.fa\n")
    system(paste("gzip -d ", fasta.file, sep=""))
    system(paste("mv ", sub(".gz$","",fasta.file)," genome.fa", sep=""))
  }
  if(gatk){
    if(length(dir[grep(sub(".vcf.gz$", "", dbsnp.file),dir)])<2){
      cat("\ndbSNP vcf.gz and/or vcf.idx.gz missing\n")
      return(2)
    }else{
      cat("\nPreparing dbsnp vcf\n")
      system(paste("gzip -d ", dbsnp.file, sep=""))
      system(paste("gzip -d ", sub(".vcf.gz$", ".vcf.idx.gz$",dbsnp.file), sep=""))
      system(paste("mv ", sub(".gz$", "",dbsnp.file), " dbsnp.vcf", sep=""))
    }
    if(length(dir[grep(sub(".vcf.gz$", "", g1000.file),dir)])<2){
      cat("\1000 genomes vcf and/or vcf.idx.gz missing\n")
      return(3)
    }else{
      cat("\nPreparing 1000 genomes vcf\n")
      system(paste("gzip -d ", g1000.file, sep=""))
      system(paste("gzip -d ", sub(".vcf.gz$", ".vcf.idx.gz$",g1000.file), sep=""))
      system(paste("mv ", sub(".gz$", "",g1000.file), " g1k.vcf", sep=""))
    }
  }

	if(group=="sudo"){
		system("sudo docker pull docker.io/rcaloger/bwa.2017.01")
		system(paste("sudo docker run --privileged=true -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.2017.01 sh /bin/bwa.index.sh "," ",genome.folder, sep=""))
	}else{
		system("docker pull docker.io/rcaloger/bwa.2017.01")
		system(paste("docker run --privileged=true -v ",genome.folder,":/data/scratch"," -d docker.io/rcaloger/bwa.2017.01 sh /bin/bwa.index.sh "," ",genome.folder, sep=""))
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
	system(paste("rm ",genome.folder,"/out.info",sep=""))

}

