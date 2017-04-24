#' @title A wrapper function for sample_size_distribution function from RnaSeqSampleSize Bioconductor package
#' @description This function executes sample_size_distribution to identify the number of samples x group needed to obtain a specific statistical power
#' @param filename, a character string indicating the name of the count table file
#' @param power, expected statistical power required to detect prognostic genes
#' @param FDR, false discovery rate
#' @param genes4dispersion, an integer indicating the number of genes used in estimation of read counts and dispersion distribution
#' @param log2fold.change, an integer indicating the minimum log2 fold change for prognostic genes between two groups
#' @import RnaSeqSampleSize
#' @return a string with the requested informations. the string is also saved in a file: sample_size_evaluation.txt , power_evaluation.txt
#' @examples
#'\dontrun{
#'  sampleSize("_counts.txt", power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#'}
#' @export

sampleSize <- function(filename, power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1){
  dataset <- read.table(filename, sep="\t", header=T, row.names=1, stringsAsFactors = F)
  col.n <- strsplit(names(dataset), "_")
  if(length(col.n[[1]])==1){
      cat("\nIt seems that covariates are not added to the count table\n")
      return("error 1")
  }
  covar <- sapply(col.n, function(x)x[2])
  dataMatrixDistribution<-est_count_dispersion(dataset, group=covar)
  n.samples <- sample_size_distribution(power=power,f=FDR,distributionObject=dataMatrixDistribution, rho=2^log2fold.change, repNumber=genes4dispersion,showMessage=F)
  sample.result <- paste("To guarantee a power of ",power, " with ", " FDR ", FDR, " and log2FC ", log2fold.change," the number of samples x group is ", n.samples, "\n", sep="")
  writeLines(sample.result, "sample_size_evaluation.txt")
  return(sample.result)
 }

