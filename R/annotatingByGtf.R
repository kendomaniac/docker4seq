#' @title Annotating RSEM gene.results using ENSEMBL gtf and refGenome CRAN package
#' @description This function executes the docker container annotate.1, where refGenome is used to annotated gene.results and isoforms.results outputs from RSEM using ENSEMBL GTF annotation
#' @param rsem.folder, a character string indicating where gene.results file is located
#' @param genome.folder, a character string indicating the folder for the genome reference used for mapping and counting with \code{"rsemstar"} function. In this folder is present the GTF used for by RSEM
#' @param truncating.expected.counts, a boolean logical variable indicating if the expected counts calculated by RSEM need to be converted in integer to be compliant with differential expression Bioconductor packages as DESeq2. Default is FALSE
#' @param annotation.type, a string indicating the ENSEMBL gene_biotype to be used for annotation.
#' @return one file: annotated_genes.results, which is the annotated version of gene.results.
#' @import refGenome
#' @import utils
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/genes.results.gz")
#'     gzip -d genes.results.gz
#'     #running rsemannoByGtf
#'     rsemannoByGtf(rsem.folder=getwd(), genome.folder="/data/scratch/hg38star",
#'     truncating.expected.counts=FALSE, annotation.type=c("miRNA","protein_coding",
#'     "antisense","snRNA","snoRNA","scRNA","sRNA","macro_lncRNA","lincRNA","scaRNA"))
#' }
#'
#' @export
rsemannoByGtf <- function(rsem.folder=getwd(),
                          genome.folder="/data/scratch/hg38star",
                          truncating.expected.counts=FALSE,
                          annotation.type=c("miRNA","protein_coding","antisense","snRNA","snoRNA","scRNA",
                          "sRNA","macro_lncRNA","lincRNA","scaRNA")){
  #running time 1
  ptm <- proc.time()
  #running time 1
  ######
  '%!in%' <- function(x,y)!('%in%'(x,y))
  ######
  beg <- ensemblGenome()
  basedir(beg) <- genome.folder
  read.gtf(beg, "genome.gtf")
  annotation <- extractPaGenes(beg)
  annotation <- annotation[which(annotation$gene_biotype%in%annotation.type),]
  dir <- dir(rsem.folder)
  dir <- dir[grep("^genes.results", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", getwd(), "there is not a genes.results file generated using rsemstar"))
    return(1)
  }else if(length(dir)>1){
    cat(paste("It seems that in ", getwd(), "there are more than one genes.results files"))
    return(2)
  }
  gene.df <- read.table(dir, sep="\t", header=T,stringsAsFactors = F)
  if(truncating.expected.counts){
    gene.df$expected_count <- trunc(gene.df$expected_count)
  }
  gene.df.ann <- gene.df[which(gene.df$gene_id%in%annotation$gene_id),]
  gene.df.ann <- gene.df.ann[order(gene.df.ann$gene_id),]
  gene.df.noann <- gene.df[which(gene.df$gene_id%!in%annotation$gene_id),]
  annotation <- annotation[which(annotation$gene_id%in%gene.df.ann$gene_id),]
  annotation <- annotation[order(annotation$gene_id),]
  if(identical(gene.df.ann$gene_id, annotation$gene_id)){
    gene.df.ann <- data.frame(annotation$gene_id, annotation$gene_biotype, annotation$gene_name, annotation$source, gene.df.ann[,2:dim(gene.df.ann)[2]])
    gene.df.noann <- data.frame(gene.df.noann[,1], rep(NA,dim(gene.df.noann)[1]), rep(NA,dim(gene.df.noann)[1]), rep(NA,dim(gene.df.noann)[1]), gene.df.noann[,2:dim(gene.df.noann)[2]])
    names(gene.df.noann) <- names(gene.df.ann)
    gene.df <- rbind(gene.df.ann, gene.df.noann)
    write.table(gene.df, "gtf_annotated_genes.results", sep="\t", col.names=NA)
    return(0)
  }else{
    cat("\nAnnotation and genes.results gene_ids are not identical!\n")
    return(3)
  }
}
