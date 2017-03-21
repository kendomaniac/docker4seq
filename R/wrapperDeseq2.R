#' @title A wrapper function for deseq2 for two groups only
#' @description This function runs deseq2 on a table genrated with sample2experiment having
#' the covariates added in the names of the columns, separated by the names with underscore.
#' @param experiment.table, a character string indicating the counts table generated with
#' sample3experiment with addition of covariates
#' @param log2fc, log2fc threshold for differetially expressed genes
#' @param fdr, fdr threshold
#' @param ref.covar, covariate to be used as reference
#' @return Returns a full table of differentially expressed genes (prefix DEfull), a filtered table
#' of differentially expressed genes (prefix DEfiltered) and the normalized counts table (prefix normalized)
#' @import DESeq2
#' @examples
#'\dontrun{

#'     wrapperDeseq2(experiment.table=paste(path.package("docker4seq"),
#'     "data/4t1_counts.txt", sep="/"),
#'     log2fc=1, fdr=0.1, ref.covar="0")
#'
#' }
#' @export

wrapperDeseq2 <- function(experiment.table, log2fc=1, fdr=0.1, ref.covar="0"){
   counts <- read.table(experiment.table, sep="\t", header=T, row.names=1)
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
   write.table(res, paste("DEfull_",experiment.table,sep=""), sep="\t", col.names = NA, quote=FALSE)
   res.filtered0 <- res[!is.na(res$padj),]
   res.filtered1 <- res.filtered0[intersect(which(res.filtered0$padj <= fdr), which(res.filtered0$log2FoldChange >= log2fc)),]
   write.table(res.filtered1, paste("DEfiltered_log2fc_",log2fc,"_fdr_",fdr,"_",experiment.table,sep=""), sep="\t", col.names = NA, quote=FALSE)
   norm.counts <- counts(dds, normalize=T)
   write.table(norm.counts, paste("log2normalized_counts_",experiment.table,sep=""), sep="\t", col.names = NA, quote=FALSE)
   plotMA(res)
   abline(h=log2fc, col="red", lty=2)
   abline(h=-log2fc, col="green", lty=2)
}
