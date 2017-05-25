#' @title A wrapper function for deseq2 for two groups only
#' @description This function runs deseq2 on a table genrated with sample2experiment having the covariates added in the names of the columns, separated by the names with underscore.
#' @param experiment.table, a character string indicating the counts table generated with sample3experiment with addition of covariates
#' @param log2fc, log2fc threshold for differetially expressed genes
#' @param fdr, fdr threshold
#' @param ref.covar, covariate to be used as reference
#' @param type, character with three options: gene, isoform, mirna. if gene is used two files are generated for geneset enrichment, the filtered Gene symbols and the background that contains all gene simbols.
#' @param output.folder, output folder
#' @param batch, logical FALSE, TRUE
#' @return Returns a full table of differentially expressed genes (prefix DEfull), a filtered table of differentially expressed genes (prefix DEfiltered) and the normalized counts table (prefix normalized)
#' @import DESeq2
#' @examples
#'\dontrun{
#'     system(paste("cp ", path.package("docker4seq"),
#'     "/examples/4t1_counts.txt .", sep=""))
#'     wrapperDeseq2(experiment.table="4t1_counts.txt",
#'     log2fc=1, fdr=0.1, ref.covar="0", type="gene")
#'
#' }
#' @export
wrapperDeseq2 <- function(experiment.table, log2fc=1, fdr=0.1, ref.covar="0", type=c("gene","isoform","mirna"),output.folder=getwd(), batch=FALSE){
 counts <- read.table(experiment.table, sep="\t", header=T, row.names=1, batch=FALSE)
 if(batch==FALSE){
   covar.tmp <- strsplit(names(counts), "_")
   if(length(covar.tmp[[1]])==1){
      cat("\nIt seems that covariates are not present in the samples name or they are not separated with underscore\n")
      return(1)
   }
   id <- sapply(covar.tmp, function(x)x[1])
   covar <- sapply(covar.tmp, function(x)x[2])
   target <- data.frame(id, covar)
   rownames(target) <- id
   names(counts) <- id
   dds <- DESeqDataSetFromMatrix(countData = counts, colData = target, design = ~covar)
   dds$covar <- relevel(dds$covar, ref=ref.covar)
   dds <- DESeq(dds)
   res <- results(dds)
   write.table(res, paste(output.folder,"DEfull.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   res.filtered0 <- res[!is.na(res$padj),]
   res.filtered1 <- res.filtered0[intersect(which(res.filtered0$padj <= fdr), which(abs(res.filtered0$log2FoldChange) >= log2fc)),]
   write.table(res.filtered1, paste(output.folder,paste("DEfiltered_log2fc_",log2fc,"_fdr_",fdr,".txt",sep=""),sep="/"), sep="\t", col.names = NA, quote=FALSE)
   norm.counts <- log2(counts(dds, normalize=T)+1)
   if(type=="gene"){
       bkg.0 <- rownames(res)
       bkg.1 <- strsplit(bkg.0, ":")
       tmp0 <- sapply(bkg.1, function(x)x[2])
       tmp1 <- sapply(bkg.1, function(x)x[1])
       bkg.df <- data.frame(tmp0, tmp1)
       bkgf.0 <- rownames(res.filtered1)
       bkgf.1 <- strsplit(bkgf.0, ":")#this is not bkg are the genes of interest
       tmp0 <- sapply(bkgf.1, function(x)x[2])
       tmp1 <- sapply(bkgf.1, function(x)x[1])
       bkgf.df <- data.frame(tmp0, tmp1)
       write.table(bkg.df[,2], paste(output.folder,"bkg4david.txt",sep="/") , sep="\t", row.names = F, col.names = F, quote=FALSE)
       write.table(bkgf.df[,2], paste(output.folder,"genes4david.txt",sep="/") , sep="\t", row.names = F, col.names = F, quote=FALSE)
       write.table(norm.counts, paste(output.folder,"log2normalized_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }else if(type=="isoform"){
       write.table(norm.counts, paste(output.folder,"log2normalized_isoforms_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }else if(type=="mirna"){
     write.table(norm.counts, paste(output.folder,"log2normalized_miRNAs_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }
 }else if(batch==TRUE){
   covar.tmp <- strsplit(names(counts), "_")
   if(length(covar.tmp[[1]])<3){
     cat("\nIt seems that batch group is not present in the samples name or they are not separated with underscore\n")
     return(1)
   }
   id <- sapply(covar.tmp, function(x)x[1])
   covar <- sapply(covar.tmp, function(x)x[2])
   covar.batch <- sapply(covar.tmp, function(x)x[3])
   target <- data.frame(id, covar, covar.batch)
   rownames(target) <- id
   names(counts) <- id
   dds <- DESeqDataSetFromMatrix(countData = counts, colData = target, design = ~covar.batch+covar)
   dds$covar <- relevel(dds$covar, ref=ref.covar)
   dds <- DESeq(dds)
   res <- results(dds)
   write.table(res, paste(output.folder,paste("DEfull_batch_",type,".txt", sep=""),sep="/"), sep="\t", col.names = NA, quote=FALSE)
   res.filtered0 <- res[!is.na(res$padj),]
   res.filtered1 <- res.filtered0[intersect(which(res.filtered0$padj <= fdr), which(abs(res.filtered0$log2FoldChange) >= log2fc)),]
   write.table(res.filtered1, paste(output.folder,paste("DEfiltered_batch_log2fc_",log2fc,"_fdr_",fdr,"_",type,".txt",sep=""),sep="/"), sep="\t", col.names = NA, quote=FALSE)
   norm.counts <- log2(counts(dds, normalize=T)+1)
   if(type=="gene"){
     bkg.0 <- rownames(res)
     bkg.1 <- strsplit(bkg.0, ":")
     tmp0 <- sapply(bkg.1, function(x)x[2])
     tmp1 <- sapply(bkg.1, function(x)x[1])
     bkg.df <- data.frame(tmp0, tmp1)
     bkgf.0 <- rownames(res.filtered1)
     bkgf.1 <- strsplit(bkgf.0, ":")#this is not bkg are the genes of interest
     tmp0 <- sapply(bkgf.1, function(x)x[2])
     tmp1 <- sapply(bkgf.1, function(x)x[1])
     bkgf.df <- data.frame(tmp0, tmp1)
     write.table(bkg.df[,2], paste(output.folder,"bkg4david.txt",sep="/") , sep="\t", row.names = F, col.names = F, quote=FALSE)
     write.table(bkgf.df[,2], paste(output.folder,"genes4david.txt",sep="/") , sep="\t", row.names = F, col.names = F, quote=FALSE)
     write.table(norm.counts, paste(output.folder,"log2normalized_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }else if(type=="isoform"){
     write.table(norm.counts, paste(output.folder,"log2normalized_isoforms_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }else if(type=="mirna"){
     write.table(norm.counts, paste(output.folder,"log2normalized_miRNAs_counts.txt",sep="/"), sep="\t", col.names = NA, quote=FALSE)
   }
 }

}
