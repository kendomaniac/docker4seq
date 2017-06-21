#' @title Plotting the distribution of zeros in cells eliminating all genes without counts
#' @description This function plots the zeros distributions in cells and removes genes without counts
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of tab delimited file  file of cells un-normalized expression counts
#' @return a PDF providing zeros distributions before removal of all genes without counts, a file with the pre
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#'     system("gzip -d singlecells_counts.txt.gz")
#'     filterZeros(data.folder=getwd(),counts.matrix="singlecells_counts.txt")
#' }
#' @export
filterZeros <- function(data.folder=getwd(), counts.matrix){
  counts <- read.table(counts.matrix, sep="\t", header=T, row.names = 1)
  counts.sum <- apply(counts, 1, function(x){
    length(which(x > 0))
  })
  counts.n0 <- counts[which(counts.sum > 0),]
  cat("\n",paste("Out of ", dim(counts)[1]," genes ",dim(counts.n0)[1]," are left after removing genes with no counts", sep=""),"\n")
  pdf(paste("zeros distribution_",sub(".txt$","",counts.matrix),".pdf",sep=""))
      hist(log10(counts.sum), xlab="log10 # cells wo zeros")
  dev.off()
  write.table(counts.n0, paste("filtered_",counts.matrix,sep=""), sep="\t", col.names=NA)
}
