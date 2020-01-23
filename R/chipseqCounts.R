#' @title Running MACS & SICER workflow NOT READY FOR STABLE check it!
#' @description This function executes a set of docker containers allowing the detection of TFs and Histon marks peaks.
#' #params skewer
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param output.folder, a character string indicating where final results will be saved
#' @param mock.folder, a character string indicating where gzip fastq file for unspecific ChIP is located
#' @param test.folder, a character string indicating where gzip fastq file for specific ChIP is located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param adapter5, a character string indicating the fwd adapter
#' @param adapter3, a character string indicating the rev adapter
#' @param threads, a number indicating the number of cores to be used from the application
#' @param seq.type, a character string indicating the type of reads to be trimmed. One options: \code{"se"} for single end sequencing
#' @param min.length, a number indicating minimal length required to return a trimmed read
#' @param genome.folder, a character string indicating the folder where the indexed reference genome is located
#' @param mock.id, a character string indicating the unique id to be associated to the mock bam that will be created
#' @param test.id, a character string indicating the unique id to be associated to the test bam that will be created
#' @param genome, a character string indicating the genome used as reference for data generation. Available options: hg19, hg38, mm9, mm10
#' @param read.size, an integer indicating the length of the sequenced reads
#' @param tool, a character string indicating the peaks calling algorith. Available options: macs and sicer. Macs, v 1.14, is used to call TF peaks, as instead sicer, v 1.1, is used to call histone mark peaks
#' @param macs.min.mfold, an integer indicating the minimum enrichment ratio against background
#' @param macs.max.mfold, an integer indicating the maximum enrichment ratio against background
#' @param macs.pval, a character string, indicationg the pvalue cutoff to be used to filter peaks with low statistical significance.The number must be provided in scientific notation as the default value shows
#' @param sicer.wsize, an integer indicating the windows size to be used by sicer
#' @param sicer.gsize, an integer indicating the gap size to be used by sicer. Suggested values: H3K4Me3=200; H3K27Me3=600
#' @param sicer.fdr, an integer indicating the pvalue cutoff to be used to filter peaks with low statistical significance
#' @param tss.distance, an integer indicating the distance of TSS with respect to gene start
#' @param max.upstream.distance, an integer indicating the maximum distance to associate a gene ID to a peak
#' @param remove.duplicates, a character string indicating if duplicated reads have to be removed. Available options: Y, to remove douplicates, N to keep duplicates
#' @author Raffaele Calogero

#' @return Returns the output of skewer, bwa, chipseq
#' @examples
#'\dontrun{
#' system("wget 130.192.119.59/public/test.chipseqCounts.zip")
#' unzip("test.chipseqCounts.zip")
#' setwd("test.chipseqCounts")
#' library(docker4seq)
#' chipseqCounts(group = "docker", output.folder = "/data/tests/chipseqCounts/test.chipseqCounts/prdm51.igg",
#'              mock.folder="/data/tests/chipseqCounts/test.chipseqCounts/igg",
#'              test.folder="/data/tests/chipseqCounts/test.chipseqCounts/prdm51", scratch.folder="/data/scratch/",
#'              adapter5 = "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#'              adapter3 = "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
#'              threads = 8, min.length = 30, genome.folder="/data/genomes/mm10bwa",
#'              mock.id = "igg", test.id = "tf", genome="mm10", read.size = 50,
#'              tool = "macs", macs.min.mfold = 10, macs.max.mfold = 30,
#'              macs.pval = "1e-5", sicer.wsize = 200, sicer.gsize = 200,
#'              sicer.fdr = 0.1, tss.distance = 0, max.upstream.distance = 10000,
#'              remove.duplicates = "N")
#' }
#' @export
chipseqCounts <- function( group=c("sudo","docker"),output.folder=getwd(), mock.folder, test.folder, scratch.folder,
                          adapter5="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
                          adapter3="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
                          threads=8, seq.type = "se", min.length=30,genome.folder,
                          mock.id="igg", test.id="tf", genome, read.size=50,
                          tool="macs", macs.min.mfold=10, macs.max.mfold=30, macs.pval="1e-5",
                          sicer.wsize=200, sicer.gsize=200, sicer.fdr=0.10,
                          tss.distance=0, max.upstream.distance=10000, remove.duplicates="N"){

  # FastQC
  home <- getwd()
  setwd(output.folder)
  fastqc(group="docker", data.folder=mock.folder)
  setwd(output.folder)
  fastqc(group="docker", data.folder=test.folder)
  setwd(output.folder)
  #trimming adapter and bwa
  cat("\nrunning skewer ctrl\n")
  check.skewer <- dir(mock.folder)
  if(length(grep("trimmed", check.skewer))==0){
       skewer(group=group,fastq.folder=mock.folder, scratch.folder=scratch.folder, adapter5=adapter5, adapter3=adapter3, seq.type="se", threads=threads,  min.length=min.length)
  }else{
    cat("\nskewer ctrl already done\n")
  }
  check.skewer <- NULL
  cat("\nrunning bwa ctrl\n")
  check.bwa <- dir(mock.folder)
  if(length(grep("dedup", check.bwa))==0){
     bwa(group=group,fastq.folder=mock.folder, scratch.folder=scratch.folder, genome.folder=genome.folder, seq.type="se", threads=threads, sample.id=mock.id)
  }else{
    cat("\nbwa ctrl already done\n")
  }
  check.bwa <- NULL
  cat("\nrunning skewer test\n")
  check.skewer <- dir(test.folder)
  if(length(grep("trimmed", check.skewer))==0){
     skewer(group=group,fastq.folder=test.folder, scratch.folder=scratch.folder, adapter5=adapter5, adapter3=adapter3, seq.type="se", threads=threads,  min.length=min.length)
  }else{
    cat("\nskewer test already done\n")
  }
  cat("\nrunning bwa test\n")
  check.bwa <- dir(test.folder)
  if(length(grep("dedup", check.bwa))==0){
     bwa(group=group,fastq.folder=test.folder, scratch.folder=scratch.folder, genome.folder=genome.folder, seq.type="se", threads=threads, sample.id=test.id)
  }else{
    cat("\nbwa test already done\n")
  }
  cat("\nmoving the bam files in the output folder\n")
  file.copy(from=paste(mock.folder,"/dedup_reads.bam", sep=""), to=paste(output.folder, "/ctrl.bam", sep=""))
  file.copy(from=paste(mock.folder,"/dedup_reads.bai", sep=""), to=paste(output.folder, "/ctrl.bai", sep=""))
  file.copy(from=paste(test.folder,"/dedup_reads.bam", sep=""), to=paste(output.folder, "/sample.bam", sep=""))
  file.copy(from=paste(test.folder,"/dedup_reads.bai", sep=""), to=paste(output.folder, "/sample.bai", sep=""))

  #running chipseq
  cat("\nrunning chipseq function\n")
  chipseq(group=group, bam.folder=output.folder, sample.bam="sample.bam", ctrl.bam="ctrl.bam",
          scratch.folder=scratch.folder, genome=genome, read.size=read.size,
          tool=tool, sicer.wsize=sicer.wsize, sicer.gsize=sicer.gsize, sicer.fdr=sicer.fdr,
          tss.distance=tss.distance, max.upstream.distance=max.upstream.distance,remove.duplicates=remove.duplicates)

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",output.folder, sep=""))
  setwd(home)
}


