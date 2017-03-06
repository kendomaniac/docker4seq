#' @title generating counts, FPKM and TPM tables from rnaseqCounts outuputs
#' @description This function generates counts, FPKM and TPM tables from rnaseqCounts outuputs. IMPORTANT the experiment.folder should contain only results folders coming from rnaseqCounts function
#' @param experiment.folder, a character string indicating the path where the rnaseqCouts output folders are present
#' @param bio.type, a character string indicating the ensemb bio.type. Options: "protein_coding","unitary_pseudogene","unprocessed_pseudogene","processed_pseudogene", "transcribed_unprocessed_pseudogene","processed_transcript","antisense","transcribed_unitary_pseudogene","polymorphic_pseudogene","lincRNA","sense_intronic","transcribed_processed_pseudogene","sense_overlapping","IG_V_pseudogene","pseudogene","TR_V_gene","3prime_overlapping_ncRNA","IG_V_gene","bidirectional_promoter_lncRNA","snRNA","miRNA","misc_RNA","snoRNA","rRNA","IG_C_gene","IG_J_gene","TR_J_gene","TR_C_gene","TR_V_pseudogene","TR_J_pseudogene","IG_D_gene","ribozyme","IG_C_pseudogene","TR_D_gene","TEC","IG_J_pseudogene","scRNA","scaRNA","vaultRNA","sRNA","macro_lncRNA","non_coding","IG_pseudogene"
#' @param output.prefix, a character value indicating the prefix to be used in the output files
#'
#' @return Returns counts, fpkm, tpm data frames for gene and isoforms, save data frames in experiment.tables.Rda, in counts.txt, log2fpkm.txt and in log2TPM
#' @examples
#'\dontrun{
#'     sample2experiment(experiment.folder=getwd(), bio.type="protein_coding", output.prefix="")
#' }
#' @export
sample2experiment <- function(experiment.folder=getwd(), bio.type=c("protein_coding","unitary_pseudogene",
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
                                                           "IG_pseudogene"), output.prefix=""){
  data.dir <- experiment.folder
  ls.folders <- dir(data.dir)

  tmp.remove <- c(grep(".Rda$",ls.folders), grep(".txt$",ls.folders),grep(".R$",ls.folders),grep(".pdf$",ls.folders))
  ls.folders <- ls.folders[setdiff(seq(1,length(ls.folders)),tmp.remove)]
  if(length(ls.folders)<2){
    cat("\nThere are less than two samples in the present folder\n")
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
      return(2)
    }else if(length(grep("gtf_annotated_genes.results", dir.tmp))==0){
      cat(paste("\nFolder ", i," does not contains gtf_annotated_genes.results\n", sep=""))
      if(length(grep("^genes.results$", dir.tmp))==0){
         cat(paste("\nFolder ", i," does not contains gtf_annotated_genes.results and genes.results\n", sep=""))
        return(3)
      }
      genes <- "genes.results"
    }
    tmp.c <- read.table(paste(data.dir, i,
                              genes,sep="/"),
                        sep="\t", header=TRUE,
                        stringsAsFactors = FALSE,
                        row.names = 1)
    tmp.c <- tmp.c[which(tmp.c$annotation.gene_biotype %in% bio.type),]
    counts[[i]] <- tmp.c$expected_count
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

  names(counts) <- ls.folders
  names(fpkm) <- ls.folders
  names(tpm) <- ls.folders
  save(counts, fpkm, tpm, file=paste(output.prefix,"_experiment.tables.Rda", sep=""))
  write.table(counts, paste(output.prefix,"_counts.txt", sep=""), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(fpkm+1), paste(output.prefix,"_log2FPKM.txt", sep=""), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(tpm+1), paste(output.prefix,"_log2TPM.txt", sep=""), sep="\t", col.names=NA, quote = FALSE)
}



