#' @title A function to selectec top X on the basis of gene/transcript expression
#' @description This function select the X top genes give a user defined threshold
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param file.name, counts table name, tab delimited. Matrix data file must be in data.folder.
#' @param threshold, integer used for filtering indicate the number of top expressed genes to be selected
#' @param logged, boolean TRUE or FALSE, if FALSE gene expression data are expressed in the plot as log10.
#'
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, UNITO
#'
#' @return a filtered tab delimited file and a histogram of the gene by gene total expression
#'
#' @examples
#'\dontrun{
#'
#'  topx(data.folder=getwd(),file.name="singlecells_counts.txt",threshold=10000, logged=FALSE)
#' }
#'
#' @export
topx <- function(data.folder ,file.name, threshold, logged=FALSE){
  home <- getwd()
  setwd(data.folder)
  tmp <- read.table(file.name, header=T, sep="\t", row.names=1)
  sum.counts <- apply(tmp, 1, sum)
  tmp <- data.frame(tmp, sum.counts)
  tmp <- tmp[order(tmp$sum.counts, decreasing = T),]

  pdf("gene_expression_distribution.pdf")
  if(!logged){
    hist(log10(tmp$sum.counts), col=rgb(1,0,0,0.5), xlab="log10 gene counts summary", breaks=100)
    hist(log10(tmp$sum.counts[1:threshold]), col=rgb(0,0,1,0.5), add=T, breaks=100)
    legend("topright",legend=c("All","Filtered"), pch=c(15,15), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
    box()
  }else{
    hist(tmp$sum.counts, col=rgb(1,0,0,0.5), xlab="log gene counts summary", breaks=100)
    hist(tmp$sum.counts[1:threshold], col=rgb(0,0,1,0.5), add=T, breaks=100)
    legend("topright",legend=c("All","Filtered"), pch=c(15,15), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
    box()
  }
  dev.off()

  tmp.filter <- tmp[1:threshold,1:(dim(tmp)[2]-1)]
  sum.counts.f <- apply(tmp.filter, 1, sum)

  write.table(tmp.filter, paste(sub(".txt",paste("_",threshold,".txt", sep=""),file.name), sep=""), sep="\t", col.names = NA)


  setwd(home)

}
