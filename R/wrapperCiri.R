#' @title Wrapper function for circRNAs prediction using CIRI 2
#' @description This function calls sequentially the docker containers for FASTQC, BWA, and CIRI to predict the list of circRNAs starting from the raw RNA-Seq reads
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param data.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.file, a character string indicating the path to the Fasta file of the reference genomic sequence (it should be the same reference indexed for the BWA alignment)
#' @param seq.type, a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing
#' @param sample.id, a character string indicating the unique id to be associated to the bam that will be created
#' @param threads, a number indicating the number of cores to be used from the application
#' @param annotation.file, a character string indicating the path to the GTF/GFF file reporting the reference gene annotations
#' @param max.span, an integer reporting the maximum spanning distance of a circRNA (default = 200000 bp)
#' @param stringency.value, the selected stringency level of the analysis. Three possible options are available: "high" (high stringency, default), in which CIRI2 only provides circRNAs supported by more than 2 distinct PCC signals; "low" (low stringency), CIRI2 only provides circRNAs supported by more than 2 junction reads; "zero", CIRI2 provides all circRNAs regardless junction read counts or PCC signals
#' @param quality.threshold, integer indicating the threshold for mapping quality of each segment of junction reads (default=10)
#' @author Nicola Licheri and Giulio Ferrero
#'
#' @return The list of circRNAs predicted by CIRI starting from the raw RNA-Seq datasets
#' @examples
#' \dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #retrieve the data of the indexed genome (chromosome 21 of hg38 human genome assembly)
#'     system("unzip master.zip")
#'     system("unzip ./circhunter-master/CircHunter/data/hg38.chr21.fa.zip")
#'     system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_1.fastq.gz") #retrieve the RNA-Seq data
#'     system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_2.fastq.gz") #retrieve the RNA-Seq data
#'
#'     #running the wrapperCiri function
#' wrapperCiri(group = "docker", scratch.folder="/data/scratch", data.folder=getwd(), genome.file="./circhunter-master/CircHunter/data/hg38.chr21.fa", seq.type = "pe", sample.id="test", threads = 1, max.span = 200000, stringency.value = "high", quality.threshold = 10) 
#'
#' }
#' @export

wrapperCiri <- function(group = c("sudo", "docker"), scratch.folder, data.folder, genome.file, seq.type = c("se", "pe"), sample.id, threads = 1, annotation.file = "", max.span = 200000, stringency.value = c("high", "low", "zero"), quality.threshold = 10) {

  # storing the position of the home folder
  home <- getwd()
  setwd(data.folder)

  # FastQC
  cat("\nrunning FastQC\n")
  fastqc(group = "docker", data.folder = data.folder)

  # Alignment with BWA
  genome.folder <- dirname(genome.file)
  
  cat("\nrunning BWA\n")
  bwa(
    group = group, scratch.folder = scratch.folder, fastq.folder = data.folder,
    genome.folder = genome.folder, seq.type = seq.type, threads = threads, sample.id = sample.id, circRNA = TRUE
  )

  # circRNA prediction with Ciri2
  cat("\nrunning CIRI2\n")
  ciri2(
    group = group, scratch.folder = scratch.folder, sam.file = paste(data.folder, "aligned_reads.sam", sep="/"),
    genome.file = genome.file, annotation.file = annotation.file,
    max.span = max.span, stringency.value = stringency.value,
    quality.threshold = quality.threshold, threads = threads
  )

  # get output .ciri file
  cirifile <- list.files(pattern = "*[.]ciri")[1]

  if (is.na(cirifile)) {
    cat(paste("\nERROR: it seems that there is no ciri output file\n"))
    system("echo 2 > ExitStatusFile 2>&1")
    setwd(home)
    return(2)
  }

  # rename .ciri file as "sample.id".ciri
  system(paste("mv prediction.ciri ", sample.id, ".ciri", sep = ""))
  
  setwd(home)
}
