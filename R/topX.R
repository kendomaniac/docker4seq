#' @title A function to selectec top X on the basis of gene/transcript expression
#' @description This function select the X top genes give a user defined threshold
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param file.name, counts table name, tab delimited. Matrix data file must be in data.folder.
#' @param threshold, integer used for filtering indicate the number of top expressed genes to be selected
#' @param log, boolean TRUE or FALSE, if FALSE gene expression data are expressed in the plot as log10.
#'
#' @author Name Family name, myemail [at] somewhere [dot] org, Affiliation
#'
#' @return a filtered tab delimited file and a histogram of the gene by gene total expression
#'
#' @examples
#'\dontrun{
#'
#'  topx(data.folder="path/of/data/folder",file.name="matrixName",threshold=100000, log=FALSE)
#' }
#'
#' @export
topx <- function(data.folder ,file.name, threshold, log=FALSE){
  home <- getwd()
  setwd(data.folder)
  tmp <- read.table(file.name, header=T, sep="\t", row.names=1)
  sum.counts <- apply(tmp, 1, sum)
  pdf("gene_expression_distribution.pdf")
  if(!log10){
    hist(log10(sum.counts), xlab="log10 gene counts summary", breaks=100)
  }else{
    hist(sum.counts, xlab="log gene counts summary", breaks=100)
  }
  dev.off()
  tmp <- tmp[order(sum.counts, decreasing = T),]

  tmp.filter <- tmp[,1:threshold]
  write.table(tmp.filter, paste(file.name, sub(".txt",paste("_",threshold,".txt", sep=""),file.name), sep=""), sep="\t", col.names = NA)
  setwd(home)

}
