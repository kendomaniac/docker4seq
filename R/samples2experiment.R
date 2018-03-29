#' @title generating counts, FPKM and TPM tables from rnaseqCounts outuputs
#' @description This function generates counts, FPKM and TPM tables from rnaseqCounts outuputs.
#' @param sample.folders, a character string indicating the paths of rnaseqCouts output folders
#' @param covariates, a character string indicating the covariates associated to each sample. Covariates are required for differnetial expression analysis
#' @param batch, a character string indicating the batch associated to each sample
#' @param bio.type, a character string indicating the ensemb bio.type. Options: "protein_coding","unitary_pseudogene","unprocessed_pseudogene","processed_pseudogene", "transcribed_unprocessed_pseudogene","processed_transcript","antisense","transcribed_unitary_pseudogene","polymorphic_pseudogene","lincRNA","sense_intronic","transcribed_processed_pseudogene","sense_overlapping","IG_V_pseudogene","pseudogene","TR_V_gene","3prime_overlapping_ncRNA","IG_V_gene","bidirectional_promoter_lncRNA","snRNA","miRNA","misc_RNA","snoRNA","rRNA","IG_C_gene","IG_J_gene","TR_J_gene","TR_C_gene","TR_V_pseudogene","TR_J_pseudogene","IG_D_gene","ribozyme","IG_C_pseudogene","TR_D_gene","TEC","IG_J_pseudogene","scRNA","scaRNA","vaultRNA","sRNA","macro_lncRNA","non_coding","IG_pseudogene"
#' @param output.prefix, a character value indicating the output folder path
#' @author Raffaele Calogero
#'
#' @return Returns counts, fpkm, tpm data frames for gene and isoforms, save data frames in experiment.tables.Rda, in counts.txt, log2fpkm.txt and in log2TPM
#' @examples
#'\dontrun{
#'   system("wget http://130.192.119.59/public/test.samples2experiment.zip")
#'   unzip("test.samples2experiment.zip")
#'   setwd("test.samples2experiment")
#'   library(docker4seq)
#'   sample2experiment(sample.folders=c("./e1g","./e2g","./e3g",
#'                  "./p1g", "./p2g", "./p3g"),
#'                  covariates=c("Cov.1","Cov.1","Cov.1",
#'                  "Cov.2","Cov.2","Cov.2"),
#'                  bio.type="protein_coding", output.prefix=".")
#' }
#' @export
sample2experiment <- function(sample.folders, covariates, batch=NULL, bio.type=c("protein_coding","unitary_pseudogene",
                                                           "unprocessed_pseudogene","processed_pseudogene",
                                                           "transcribed_unprocessed_pseudogene","processed_transcript",
                                                           "antisense","transcribed_unitary_pseudogene",
                                                           "polymorphic_pseudogene","lincRNA",
                                                           "sense_intronic","transcribed_processed_pseudogene",
                                                           "sense_overlapping","IG_V_pseudogene",
                                                           "pseudogene","TR_V_gene",
                                                           "3prime_overlapping_ncRNA","IG_V_gene",
                                                           "bidirectional_promoter_lncRNA","snRNA",
                                                           "miRNA","misc_RNA",
                                                           "snoRNA","rRNA",
                                                           "IG_C_gene","IG_J_gene",
                                                           "TR_J_gene","TR_C_gene",
                                                           "TR_V_pseudogene","TR_J_pseudogene",
                                                           "IG_D_gene","ribozyme",
                                                           "IG_C_pseudogene","TR_D_gene",
                                                           "TEC","IG_J_pseudogene",
                                                           "scRNA","scaRNA",
                                                           "vaultRNA","sRNA",
                                                           "macro_lncRNA","non_coding",
                                                           "IG_pseudogene"), output.prefix="."){
  
  
  #initialize status
  system(paste("echo 0 >&",output.prefix ,"/ExitStatusFile",sep=""))
  
#preparing covar
  if( length(sample.folders)!=length(covariates)){
      cat("\nCovariates and sample folders have not the same length\n")
      system(paste("echo 2 >&",output.prefix ,"/ExitStatusFile",sep=""))
      return(2)
  }else{
    tmp.samples <- strsplit(sample.folders, "/")
    tmp.samples <- sapply( tmp.samples, function(x)x[length(x)])
    ls.names <- paste(tmp.samples, covariates, sep="_")
  }
#preparing batch
  if(!is.null(batch) & length(unique(batch)) > 1){
    if( length(sample.folders)!=length(batch)){
       cat("\nBatch and sample folders have not the same length\n")
       system(paste("echo 4 >&",output.prefix ,"/ExitStatusFile",sep=""))
       return(4)
    }else{
        ls.names <- paste(ls.names, batch, sep="_")
    }
  }
  ls.folders <- sample.folders
  if(length(ls.folders)<2){
    cat("\nThere are less than two samples in the present folder\n")
    system(paste("echo 1 >&",output.prefix ,"/ExitStatusFile",sep=""))
    return(1)
  }

  counts <- list()
  fpkm <- list()
  tpm <- list()
  for(i in ls.folders){
    genes <- "gtf_annotated_genes.results"
    dir.tmp <- dir(i)
    if(length(grep("genes.results$", dir.tmp))==0){
      cat(paste("\nFolder ", i," does not contains a rnaseqCounts output\n", sep=""))
      system(paste("echo 2 >&",output.prefix ,"/ExitStatusFile",sep=""))
      return(2)
    }else if(length(grep("gtf_annotated_genes.results", dir.tmp))==0){
      cat(paste("\nFolder ", i," does not contains gtf_annotated_genes.results\n", sep=""))
      if(length(grep("^genes.results$", dir.tmp))==0){
         cat(paste("\nFolder ", i," does not contains gtf_annotated_genes.results and genes.results\n", sep=""))
        system(paste("echo 3 >&",output.prefix ,"/ExitStatusFile",sep=""))
        return(3)
      }
      genes <- "genes.results"
    }
    tmp.c <- read.table(paste(i,genes,sep="/"),
                        sep="\t", header=TRUE,
                        stringsAsFactors = FALSE,
                        row.names = 1)
    tmp.c <- tmp.c[which(tmp.c$annotation.gene_biotype %in% bio.type),]
    counts[[i]] <- trunc(as.numeric(tmp.c$expected_count))
    fpkm[[i]] <- tmp.c$FPKM
    tpm[[i]] <- tmp.c$TPM
  }
  #adding gene names and gene id to table
  counts <- as.data.frame(counts)
  fpkm <- as.data.frame(fpkm)
  tpm <- as.data.frame(tpm)
  counts.names <- paste(tmp.c$annotation.gene_name,
                        tmp.c$annotation.gene_id, sep=":")
  rownames(counts) <- counts.names
  rownames(fpkm) <- counts.names
  rownames(tpm) <- counts.names

  names(counts) <- ls.names
  names(fpkm) <- ls.names
  names(tpm) <- ls.names

  # creating also transcripts tables
  counts.iso <- list()
  fpkm.iso <- list()
  tpm.iso <- list()
  for(i in ls.folders){
    iso <- "isoforms.results"
    tmp.c <- read.table(paste(i,iso,sep="/"),
                        sep="\t", header=TRUE,
                        stringsAsFactors = FALSE,
                        row.names = 1)
    counts.iso[[i]] <- trunc(tmp.c$expected_count)
    fpkm.iso[[i]] <- tmp.c$FPKM
    tpm.iso[[i]] <- tmp.c$TPM
  }
  #adding iso names and iso id to table
  counts.iso <- as.data.frame(counts.iso)
  fpkm.iso <- as.data.frame(fpkm.iso)
  tpm.iso <- as.data.frame(tpm.iso)
  counts.iso.names <- rownames(tmp.c)
  rownames(counts.iso) <- counts.iso.names
  rownames(fpkm.iso) <- counts.iso.names
  rownames(tpm.iso) <- counts.iso.names

  names(counts.iso) <- ls.names
  names(fpkm.iso) <- ls.names
  names(tpm.iso) <- ls.names


  save(counts, fpkm, tpm, counts.iso, fpkm.iso, tpm.iso, file=paste(output.prefix,"_experiment.tables.Rda", sep="/"))

  write.table(counts, paste(output.prefix,"_counts.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(fpkm+1), paste(output.prefix,"_log2FPKM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(tpm+1), paste(output.prefix,"_log2TPM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)

  write.table(counts.iso, paste(output.prefix,"_isoforms_counts.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(fpkm.iso+1), paste(output.prefix,"_isoforms_log2FPKM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(tpm.iso+1), paste(output.prefix,"_isoforms_log2TPM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)


}



