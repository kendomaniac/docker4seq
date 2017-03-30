#' @title generating a PCA from counts, FPKM and TPM tables from rnaseqCounts outuputs
#' @description This function generates PCA plot from counts, FPKM and TPM tables from rnaseqCounts outuputs.
#' @param experiment.table, a character string indicating the counts, FPKM or TPM table file name
#' @param type, a character value indicating the content of the file: counts, FPKM or TPM
#' @param covariatesInNames, a boolean value indicating if covariates are inserted after \_ in the filename
#' @param principal.components, a numerical vector with two values indicating the principal components to be plotted
#' @param legend.position, a character string indicating the location of the covariates legend
#' @param pdf, a boolean value indicating if results has to be saved in a pdf
#' @return Returns a PCA plot
#' @examples
#'\dontrun{
#'     # Example of the header of the counts, FPKM or TPM contaning covariate
#'     # no covariate in the name ctrl1 ctrl2 trt1 trt2
#'     # no covariate in the name ctrl1_c ctrl2_c trt1_t trt2_t
#'     pca(experiment.table="_log2FPKM.txt", type="FPKM")
#'
#' }
#' @export

pca <- function(experiment.table="_counts.txt", type=c("counts","FPKM","TPM"),
                covariatesInNames=FALSE, principal.components=c(1,2),
                legend.position=c("bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), pdf=TRUE){
  tmp <- read.table(experiment.table, sep="\t", stringsAsFactors = TRUE, header=T, check.names = FALSE, row.names=1)
  if(covariatesInNames){
    covar.tmp <- strsplit(names(tmp), '_')
    covar <- unlist(sapply(covar.tmp, function(x)x[length(x)]))
    covar <- as.factor(covar)
    sample.names <- unlist(sapply(covar.tmp, function(x)x[length(x)-1]))
  }else{
    sample.names <- names(tmp)
  }
  if(type=="counts"){
      data <- log2(tmp+1)
  }else{
     data <- tmp
  }

  pca <- prcomp(t(data))
  variance.proportion <- summary(pca)$importance[2,]
  if(pdf){
    pdf("pca.pdf")
    if(covariatesInNames){
      my.colors <- rainbow(n=length(levels(covar)))
      plot(pca$x[,principal.components], main=experiment.table, col=my.colors[covar], pch=19, xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
                                                                                                         signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
                                                                                                                                                                                                  signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))
      text(pca$x[,principal.components], label=sample.names)
      legend(legend.position, legend=levels(covar), pch=rep(19,length(levels(covar))), col=my.colors)
    }else{
      plot(pca$x[,principal.components], main=experiment.table, pch=19, col="gold", xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
                                                                                               signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
                                                                                                                                                                                        signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))
      text(pca$x[,principal.components], label=sample.names)
    }
    dev.off()
  }else{
   if(covariatesInNames){
     my.colors <- rainbow(n=length(levels(covar)))
     plot(pca$x[,principal.components], main=experiment.table, col=my.colors[covar], pch=19, xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
         signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
         signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))
     text(pca$x[,principal.components], label=sample.names)
     legend(legend.position, legend=levels(covar), pch=rep(19,length(levels(covar))), col=my.colors)
   }else{
     plot(pca$x[,principal.components], main=experiment.table, pch=19, col="gold", xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
         signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
         signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))
     text(pca$x[,principal.components], label=sample.names)
   }
  }
}

