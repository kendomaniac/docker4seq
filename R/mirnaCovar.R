#' @title Adding covariates and batch information to miRNAseq raw counts output
#' @description This function modifies the all.counts.txt table generated from miRNAseq adding covariates and batch information
#' @param experiment.folder, a character string indicating the full paths to miRNAseq input file, i.e. including the name of the file
#' @param covariates, a character vector indicating the covariates associated to each sample, e.g. c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.2", "Cov.2", "Cov.2", "Cov.2"). Covariates are required for differential expression analysis
#' @param batches, a character vector indicating the covariates associated to each sample, e.g. c("bath.1", "bath.1", "bath.2", "bath.2", "batch.1", "batch.1", "batch.2", "batch.2"). Batch info might be required for differential expression analysis
#' @param output.folder, a character vector indicating the paths to miRNAseq output folder
#' @author Raffaele Calogero
#'
#' @return Returns 0 and a count file, i.e. all.counts.txt, with the prefix "w_covar" or w_covar_batch depending if only covariates were added or also batch info were added
#' @examples
#'\dontrun{
#'    system("wget 130.192.119.59/public/test.mirna.analysis.zip")
#'    unzip("test.mirna.analysis.zip")
#'    setwd("test.mirna.analysis")
#'    library(docker4seq)
#'    mirnaCovar(experiment.folder=paste(getwd(), "all.counts.txt", sep="/"),
#'           covariates=c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.1",
#'                        "Cov.2", "Cov.2", "Cov.2", "Cov.2", "Cov.2", "Cov.2"),
#'           batches=c("bath.1", "bath.1", "bath.2", "bath.2", "batch.1", "batch.1",
#'                     "batch.2", "batch.2","batch.1", "batch.1","bath.2", "bath.2"), output.folder=getwd())
#' }
#' @export
mirnaCovar <- function(experiment.folder, covariates=NULL, batches=NULL, output.folder){
#       tmp <- read.table(paste(experiment.folder, "all.counts.txt", sep="/"), sep="\t", header=T, stringsAsFactors = F, row.names = 1)
  #remembering actual folder
  home <- getwd()
  #setting rsem output folder as working dir
  setwd(dirname(experiment.folder))
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
       tmp <- read.table(experiment.folder, sep="\t", header=T, stringsAsFactors = F, row.names = 1)
       if(!is.null(covariates)){
           tmp.n <- names(tmp)
           tmp.n <- sapply(strsplit(tmp.n, "_"),function(x)x[1])
           if(length(tmp.n)==length(covariates)){
                tmp.n <- paste(tmp.n,covariates, sep="_")
           }else{
             cat("\nERROR: the covariates and the samples names do not have the same length\n")
             system("echo 1 > ExitStatusFile 2>&1")
 	     setwd(home)
             return(1)
           }
       }
       if(!is.null(batches)){
         if(length(tmp.n)==length(batches)){
           tmp.n <- paste(tmp.n,batches, sep="_")
         }else{
           cat("\nERROR: the batches and the samples names do not have the same length\n")
           system("echo 2 > ExitStatusFile 2>&1")
 	   setwd(home)
           return(2)
         }
       }
       setwd(output.folder)
       if(!is.null(covariates) & !is.null(batches)){
            names(tmp) <- tmp.n
            write.table(tmp, "w_covar_batch_all.counts.txt", sep="\t", col.names = NA, quote = F)
       }else if(!is.null(covariates) & is.null(batches)){
         names(tmp) <- tmp.n
         write.table(tmp, "w_covar_all.counts.txt", sep="\t", col.names = NA, quote = F)
       }
       system("echo 0 > ExitStatusFile 2>&1")
       setwd(home)
       return(0)
}




