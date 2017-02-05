#' @title Running MACS & SICER workflow
#' @description This function executes a set of docker containers allowing the detection of TFs and Histon marks peaks.
#' #params skewer
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param fastq.mock, a character string indicating where gzip fastq files of the mock sample are located
#' @param fastq.sample, a character string indicating where gzip fastq files of the sample under analysis are located. In this folder will be saved all the output data
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param adapter5, a character string indicating the fwd adapter
#' @param adapter3, a character string indicating the rev adapter
#' @param seq.type, a character string indicating the type of reads to be generated by the sequencer. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing.
#' @param threads, a number indicating the number of cores to be used from the application
#' @param min.length, a number indicating minimal length required to return a trimmed read
#' #params bwa
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for bwa is located. IMPORTANT the present function only suport genomic indexes made using ensembl genom and the corresponding gtf
#' #params chipseq
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
#' @param sample.bam, a character string indicating the chipseq file under analysis
#' @param ctrl.bam, a character string indicating the control file, e.g. unspecific IgG, input DNA, etc.
#' @return Returns the output of skewer, bwa, chipseq
#' @examples
#'\dontrun{
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     chipseqCounts(group="sudo",fastq.mock, fastq.sample=getwd(), scratch.folder="/data/scratch",
#'     adapter5="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT",
#'     adapter3="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT",
#'     seq.type="pe", threads=10,  min.length=40,
#'     genome.folder="/data/scratch/bwa",
#'     sample.bam="YAPavCClp1.bam", ctrl.bam="YAPIgG.bam", genome="hg19", read.size=50,
#'     tool="macs", macs.min.mfold=10, macs.max.mfold=30, macs.pval="1e-5",
#'     sicer.wsize=200, sicer.gsize=200, sicer.fdr=0.10, tss.distance=0, max.upstream.distance=10000,
#'     remove.duplicates="N")
#' }
#' @export
chipseqCounts<- function( group="sudo",fastq.mock, fastq.sample=getwd(), scratch.folder="/data/scratch", threads=4,
adapter5="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT",
adapter3="AATGATACGGCGACCACCGAGATCTACACTCTTTCCCTACACGACGCTCTTCCGATCT",
seq.type="pe",   min.length=40,
genome.folder, sample.bam, ctrl.bam, genome="hg19", read.size=50, tool="macs", macs.min.mfold=10, macs.max.mfold=30, macs.pval="1e-5",
sicer.wsize=200, sicer.gsize=200, sicer.fdr=0.10, tss.distance=0, max.upstream.distance=10000, remove.duplicates="N"){
  #trimming adapter
  skewer(group=group,fastq.folder=fastq.mock, scratch.folder=scratch.folder,adapter5=adapter5, adapter3=adapter3, seq.type=seq.type, threads=threads,  min.length=min.length)
  skewer(group=group,fastq.folder=fastq.sample, scratch.folder=scratch.folder,adapter5=adapter5, adapter3=adapter3, seq.type=seq.type, threads=threads,  min.length=min.length)
  #running bwa
  bwa(group="sudo",fastq.folder=fastq.sample, scratch.folder=scratch.folder, genome.folder=genome.folder, seq.type=seq.type,threads=threads, sample.id="sample")
  bwa(group="sudo",fastq.folder=fastq.mock, scratch.folder=scratch.folder, genome.folder=genome.folder, seq.type=seq.type,threads=threads, sample.id="mock")
  out <- "xxxx"
  #waiting for the end of the bwa mock container
  while(out != "run.info"){
    Sys.sleep(10)
    cat(".")
    out.tmp <- dir(fastq.mock)
    out.tmp <- out.tmp[grep("run.info",out.tmp)]
    if(length(out.tmp)>0){
      out <- "out.info"
      system(paste("mv ",fastq.mock,"/*.bam ",fastq.sample,sep=""))
    }
  }
  #running chipseq
  chipseq(group=group, bam.folder=fastq.sample, sample.bam=sample.bam, ctrl.bam=ctrl.bam,
          scratch.folder=scratch.folder, genome=genome, read.size=read.size,
          tool=tool, sicer.wsize=sicer.wsize, sicer.gsize=sicer.gsize, sicer.fdr=sicer.fdr,
          tss.distance=tss.distance, max.upstream.distance=max.upstream.distance,remove.duplicates=remove.duplicates)
}

