#' @title A wrapper function to sample_size_distribution from RnaSeqSampleSize
#' @description This function executes sample_size_distribution to identify the number of samples x group needed to obtain a specific statistical power
#' @param filename, a character string indicating the name of the count table file
#' @param power, oower to detect prognostic genes
#' @param FDR, FDR level
#' @param genes4dispersion, number of genes used in estimation of read counts and dispersion distribution.
#' @param log2fold.change, a number indicating the minimum log2 fold changes for prognostic genes between two groups.
#' @import RnaSeqSampleSize
#' @return a string with the requested informations
#' @examples
#'\dontrun{
#'  sampleSize("_counts.txt", power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#'
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
#  est_power_distribution(n=6,f=0.1,rho=2^log2fold.change, distributionObject=dataMatrixDistribution,repNumber=genes4dispersion)
  n.samples <- sample_size_distribution(power=power,f=FDR,distributionObject=dataMatrixDistribution, rho=2^log2fold.change, repNumber=genes4dispersion,showMessage=F)
  return(paste("To guarantee a power of ",power, " with ", " FDR ", FDR, " and log2FC ", log2fold.change," the number of samples x group is ", n.samples, "\n", sep=""))
 }

experimentPower <- function(filename, replicatesXgroup=3, FDR=0.1, genes4dispersion=200, log2fold.change=1){
  dataset <- read.table(filename, sep="\t", header=T, row.names=1, stringsAsFactors = F)
  col.n <- strsplit(names(dataset), "_")
  if(length(col.n[[1]])==1){
    cat("\nIt seems that covariates are not added to the count table\n")
    return("error 1")
  }
  covar <- sapply(col.n, function(x)x[2])
  dataMatrixDistribution<-est_count_dispersion(dataset, group=covar)
  exp.power <- est_power_distribution(n=replicatesXgroup,f=FDR,rho=2^log2fold.change, distributionObject=dataMatrixDistribution,repNumber=genes4dispersion)
  return(paste("The power of the experiment with FDR=", FDR, " ,log2FC=", log2fold.change," and ", replicatesXgroup, "replicates x group is ", exp.power, "\n", sep=""))
}
