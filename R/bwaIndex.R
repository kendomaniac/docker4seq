#' @title Generating bwa genome index
#' @description This function executes the docker container bwa1 where BWA is installed. Optionally, the index can be created also for GATK bundle data genome fasta file.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa will be located
#' @param mode, a character string indicating the required type of analysis. Compatible analyses mode are "General", "GATK", "miRNA", and "ncRNA". In "General" mode the url of any online fasta file ("genome.url" argument) can be provided and indexed, only canonical cromosopmes are kept see id.fa after end of indexing. In the GATK analysis mode, the list of variants from dbsnp ("dbsnp.file" argument) and g1000 ("dbsnp.file" argument) are required in addition to the url of the genome fasta ("genome.url" argument). In "miRNA" analysis mode, the version ("mb.version" argument) and species prefix ("mb.species" argument) of miRBase are required. In "ncRNA" analysis mode, the version ("rc.version" argument) and species prefix ("rc.species" argument) of RNA Central are required. This mode require also a desidered maximum length of the studied RNA annotations ("length" argument).
#' @param genome.url, a character string indicating the URL from download web page for the genome sequence of interest
#' @param gtf.url, a character string indicating the URL from ENSEMBL ftp for the GTF for genome of interest
#' @param dbsnp.file, a character string indicating the name of dbSNP vcf located in the genome folder. The dbSNP vcf, dbsnp_138.b37.vcf.gz and dbsnp_138.hg19.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37
#' @param g1000.file, a character string indicating the name of 1000 genome vcf located in the genome folder. The 1000 genomes vcf, Mills_and_1000G_gold_standard.indels.b37.vcf.gz and Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.idx.gz, can be downloaded from ftp://gsapubftp-anonymous@ftp.broadinstitute.org/bundle/b37/
#' @param mb.version, a character string indicating the required version of miRBase database. Visit http://www.mirbase.org to select the proper version number.
#' @param mb.species, a character string indicating the name of a species annotated in miRBase (e.g. "hsa" for human miRNAs). Please refer to http://www.mirbase.org/help/genome_summary.shtml to proper species name.
#' @param rc.version, a character string indicating the required version of RNA Central database. Visit ftp://ftp.ebi.ac.uk/pub/databases/RNAcentral/releases/ to select the proper version number.
#' @param rc.species, a character string indicating the name of a species annotated in RNA Central (e.g. "Homo sapiens" for human ncRNAs). Please refer to NCBI taxonomy annotations at https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi to proper species name.
#' @param length, an integer corresponding on the length threshold selected to define the ncRNA reference from RNA Central.
#' @author Giulio Ferrero
#'
#' @return The indexed bwa reference sequence
#' @examples
#'\dontrun{
#'
#'     #running generic bwa index
#'     bwaIndex(group="docker", genome.folder="/data/genomes/mm10bwa", genome.url="ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/005/845/GCF_000005845.2_ASM584v2/GCF_000005845.2_ASM584v2_genomic.fna.gz", mode="General")
#
#'     #running bwa index for gatk
#'     bwaIndex(group="docker", genome.folder="/data/genomes", genome.url="http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz", dbsnp.file="dbsnp_138.hg19.vcf.gz", g1000.file="Mills_and_1000G_gold_standard.indels.hg19.sites.vcf.gz", mode="GATK")
#'
#'     #running bwa index for miRNA analysis
#'     bwaIndex(group="docker", genome.folder="/data/genomes", mb.version="22", mb.species="hsa", mode="miRNA")
#'
#'     #running bwa index for ncRNA analysis
#'     bwaIndex(group="docker", genome.folder="/data/genomes/hg19_bwa", rc.version="9", rc.species="Homo sapiens", length=80, mode="ncRNA")
#' }
#' @export
bwaIndex <- function(group=c("sudo","docker"), genome.folder=getwd(), genome.url=NULL, gtf.url=NULL, dbsnp.file=NULL, g1000.file=NULL, mode=c("General","GATK","miRNA","ncRNA"), mb.version=NULL, mb.species=NULL, rc.version=NULL, rc.species=NULL, length=NULL){

    genome.folder <- normalizePath(genome.folder)

  #########check genome folder exist###########
  if (!file.exists(genome.folder)){
    cat(paste("\n",genome.folder, "folder does not exist, It will be created \n"))
    if (!dir.create(genome.folder)){
      cat(paste("\nError ",genome.folder, "folder cannot be created\n"))
      system("echo 4 > ExitStatusFile 2>&1")
      return(4)
    }
  }
  #############################################

  home <- getwd()
  setwd(genome.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    #initialize status
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }

	cat("\nSetting as working dir the genome folder and running bwa docker container\n")

  if(mode=="GATK"){
    if(length(dir[grep(sub(".vcf.gz$", "", dbsnp.file),dir)])<2){
      cat("\ndbSNP vcf.gz and/or vcf.idx.gz missing\n")
      system("echo 2 > ExitStatusFile 2>&1")
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
      system("echo 3 > ExitStatusFile 2>&1")
      setwd(home)
      return(3)
    }else{
      cat("\nPreparing 1000 genomes vcf\n")
      system(paste("gzip -d ", g1000.file, sep=""))
      system(paste("gzip -d ", sub(".vcf.gz$", ".vcf.idx.gz$",g1000.file), sep=""))
      system(paste("mv ", sub(".gz$", "",g1000.file), " g1k.vcf", sep=""))
    }
 params <- paste("--cidfile ", genome.folder, "/dockerID -v ",     genome.folder,":/data/scratch", " -d docker.io/repbioinfo/bwaindex sh  /bin/bwa.index.sh ", genome.folder, " ", mode, " ", genome.url,   sep="")
  }

### BWA index case for miRNA
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
	    params <- paste("--cidfile ", genome.folder, "/dockerID -v ", genome.folder,":/data/scratch", " -d docker.io/repbioinfo/bwaindex bash /bin/bwa.index.sh ", genome.folder, " ", mode, " ", mb.version, " ", mb.species, sep="")
	  }
	}

### BWA index case for non miRNA ncRNAs
	if(mode=="ncRNA"){
	
	  rc_ok_ver = paste0(seq(1, 20, by=1), ".0")
	
	  rc.version = paste0(round(as.numeric(rc.version)), ".0")

	  if(rc.version %in% rc_ok_ver == FALSE){
	    cat("\nThe RNA Central version is not correct\n")
	    system("echo 2 > ExitStatusFile 2>&1")
	    setwd(home)
	    return(2)
	  }
	  if(is.null(rc.species)){
	    cat("\nPlease insert a proper RNA Central species identifier\n")
	    system("echo 2 > ExitStatusFile 2>&1")
	    setwd(home)
	    return(2)
	  }
	  else{
	    params <- paste("--cidfile ", genome.folder, "/dockerID -v ", genome.folder,":/data/scratch", " -d docker.io/repbioinfo/bwaindex bash /bin/bwa.index.sh ", genome.folder, " ", mode, " ", rc.version, " \"", rc.species, "\" ", length, sep="")
	  }
	}

### BWA index case for generic analysis

  if(mode=="General"){
  
  #download gtf
  system(paste("wget", gtf.url, sep=" "))
  system("gzip -d *.gz")
  params <- paste("--cidfile ", genome.folder, "/dockerID -v ",     genome.folder,":/data/scratch", " -d docker.io/repbioinfo/bwaindex bash /bin/bwa.index.sh ", genome.folder, " ", mode, " ", genome.url,   sep="")

}

  resultRun <- runDocker(group=group, params=params)

  if(resultRun==0){
    cat("\nBWA index generation is finished\n")
  }

	#running time 2
	ptm <- proc.time() - ptm
	con <- file(paste(genome.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)


	tmp.run[length(tmp.run)+1] <- paste("BWA index user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("BWA index system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("BWA index elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run, paste(genome.folder,"run.info", sep="/"))
  #running time 2

	#system(paste("rm ",genome.folder,"/out.info",sep=""))

	#saving log and removing docker container
	container.id <- readLines(paste(genome.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))

	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",genome.folder, sep=""))

	system(paste("rm ",genome.folder,"/dockerID",sep="" ))

  setwd(home)
}
