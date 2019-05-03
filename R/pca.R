#' @title generating a PCA from counts, FPKM and TPM tables from rnaseqCounts outuputs
#' @description This function generates PCA plot from counts, FPKM and TPM tables from rnaseqCounts outuputs.
#' @param experiment.table, a character string indicating the counts, FPKM or TPM table file name and its path
#' @param type, a character value indicating the content of the file: counts, FPKM or TPM
#' @param covariatesInNames, a boolean value indicating if covariates are inserted after \_ in the filename
#' @param samplesName, a boolean value indicating if in the plot samples names are plotted or not
#' @param principal.components, a numerical vector with two values indicating the principal components to be plotted
#' @param legend.position, a character string indicating the location of the covariates legend
#' @param pdf, a boolean value indicating if results has to be saved in a pdf
#' @param output.folder, output folder
#' @author Raffaele Calogero
#'
#' @return Returns a PCA plot
#' @examples
#'\dontrun{
#'   system("wget 130.192.119.59/public/test.analysis.zip")
#'   unzip("test.analysis.zip")
#'   setwd("test.analysis")
#'   library(docker4seq)
#'   pca(experiment.table="./_log2FPKM.txt", type="FPKM",
#'       legend.position="topleft", covariatesInNames=FALSE, samplesName=TRUE,
#'       principal.components=c(1,2), pdf = TRUE,
#'       output.folder=getwd())
#'
#' }
#' @export

pca <- function(experiment.table="./_counts.txt", type=c("counts","FPKM","TPM"),
                covariatesInNames=FALSE, samplesName=TRUE, principal.components=c(1,2),
                legend.position=c("bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"), pdf = TRUE, output.folder=getwd()){

  #storing the position of the home folder
  home <- getwd()
  #setting rsem output folder as working dir


  tmp <- read.table(experiment.table, sep="\t", stringsAsFactors = TRUE, header=T, check.names = FALSE, row.names=1)
  
  setwd(output.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  
  if(covariatesInNames){
    covar.tmp <- strsplit(names(tmp), '_')
    covar <- unlist(sapply(covar.tmp, function(x)x[2]))
    covar <- as.factor(covar)
    sample.names <- unlist(sapply(covar.tmp, function(x)x[1]))
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
  if(pdf == TRUE){
    file_name=paste(output.folder,"pca.pdf",sep="/")
    pdf(file_name)
    if(covariatesInNames){
      my.colors <- rainbow(n=length(levels(covar)))
      plot(pca$x[,principal.components], main=experiment.table, col=my.colors[covar], pch=19, xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
                                                                                                         signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
                                                                                                       signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))
      if(samplesName){
                 text(pca$x[,principal.components], label=sample.names)
      }

      legend(legend.position, legend=levels(covar), pch=rep(19,length(levels(covar))), col=my.colors)

    }else{
      plot(pca$x[,principal.components], main=experiment.table, pch=19, col="gold", xlab=paste(dimnames(pca$x)[[2]][principal.components[1]],' (',
                                                                                               signif(variance.proportion[principal.components[1]]*100,3),' %',')', sep=""), ylab=paste(dimnames(pca$x)[[2]][principal.components[2]],' (',
                                                                                               signif(variance.proportion[principal.components[2]]*100,3),'%',')', sep=""))

      if(samplesName){
                 text(pca$x[,principal.components], label=sample.names)
      }

    }
    grid()
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

  setwd(home)
}

