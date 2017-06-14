#' @title A wrapper function for experiment_power from RnaSeqSampleSize Bioconductor package
#' @description This function evaluate the statistical power of a pilot experiment
#' @param filename, a character string indicating the name of the count table file
#' @param replicatesXgroup, an integer indicating the number of samples used in each group
#' @param FDR, false discovery rate
#' @param genes4dispersion, an integer indicating the number of genes used in estimation of read counts and dispersion distribution
#' @param log2fold.change, an integer indicating the minimum log2 fold change for prognostic genes between two groups
#' @param output.folder, a string indicating the path where to save the output file
#' @import RnaSeqSampleSize
#' @return a string with the requested informations. The string is also saved in a file: power_evaluation.txt
#' @examples
#'\dontrun{
#'  system("wget 130.192.119.59/public/test.analysis.zip")
#'  unzip("test.analysis.zip")
#'  setwd("test.analysis")
#'  library(docker4seq)
#'  experimentPower("_counts.txt",replicatesXgroup=7, 
#'  FDR=0.1, genes4dispersion=200, log2fold.change=1)
#'}
#' @export


experimentPower <- function(filename, replicatesXgroup=3, FDR=0.1, genes4dispersion=200, log2fold.change=1,  output.folder=getwd()){
  dataset <- read.table(filename, sep="\t", header=T, row.names=1, stringsAsFactors = F)
  col.n <- strsplit(names(dataset), "_")
  if(length(col.n[[1]])==1){
    cat("\nIt seems that covariates are not added to the count table\n")
    return("error 1")
  }
  covar <- sapply(col.n, function(x)x[2])
  dataMatrixDistribution<-est_count_dispersion(dataset, group=covar)
  exp.power <- est_power_distribution(n=replicatesXgroup,f=FDR,rho=2^log2fold.change, distributionObject=dataMatrixDistribution,repNumber=genes4dispersion)
  power.result <- paste("The power of the experiment with FDR=", FDR, " ,log2FC=", log2fold.change," and ", replicatesXgroup, " replicates x group is ", exp.power, "\n", sep="")
  writeLines(power.result, paste(output.folder, "power_evaluation.txt", sep="/"))
  return(power.result)
}
