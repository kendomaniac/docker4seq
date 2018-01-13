#' @title Filter a count table using a table of DE from wrapperDeseq2
#' @description This function generates counts, FPKM and TPM tables including only the differentially expressed genes present in the set of DE generated with wrapperDeseq2.
#' @param data.folder, a character string indicating the paths of rnaseqCouts output folders
#' @param type, character with three options: gene, isoform, mirna.
#' @author Raffaele Calogero
#'
#' @return Returns counts, fpkm, tpm data frames for gene and isoforms in countsDE.txt, log2fpkmDE.txt and in log2TPMDE.txt
#' @examples
#'\dontrun{
#'     system("wget 130.192.119.59/public/test.analysis.zip")
#'     unzip("test.analysis.zip")
#'     setwd("test.analysis")
#'     library(docker4seq)
#'     wrapperDeseq2(output.folder=getwd(), group="docker", experiment.table="_counts.txt", log2fc=1,
#'     fdr=0.1, ref.covar="Cov.1", type="gene", batch=FALSE))
#'
#'     filterCounts(data.folder=getwd(), type="gene")
#'
#' }
#' @export
filterCounts <- function(data.folder, type=c("gene", "isoforms", "mirna")){
  dir <- dir()
  counts.file <- dir[grep("^_counts", dir)]



  write.table(counts, paste(output.prefix,"_counts.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(fpkm+1), paste(output.prefix,"_log2FPKM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(tpm+1), paste(output.prefix,"_log2TPM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)

  write.table(counts.iso, paste(output.prefix,"_isoforms_counts.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(fpkm.iso+1), paste(output.prefix,"_isoforms_log2FPKM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)
  write.table(log2(tpm.iso+1), paste(output.prefix,"_isoforms_log2TPM.txt", sep="/"), sep="\t", col.names=NA, quote = FALSE)


}



