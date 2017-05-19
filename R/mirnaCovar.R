#' @title Adding covariates and batch information to miRNAseq raw counts output
#' @description This function modifies the all.counts.txt table generated from miRNAseq adding covariates and batch information
#' @param experiment.folder, a character string indicating the paths to miRNAseq output folder
#' @param covariates, a character vector indicating the covariates associated to each sample, e.g. c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.2", "Cov.2", "Cov.2", "Cov.2"). Covariates are required for differential expression analysis
#' @param batch, a character vector indicating the covariates associated to each sample, e.g. c("bath.1", "bath.1", "bath.2", "bath.2", "batch.1", "batch.1", "batch.2", "batch.2"). Batch info might be required for differential expression analysis
#'
#' @return Returns 0 and a count file, i.e. all.counts.txt, with the prefix "w_covar" or w_covar_batch depending if only covariates were added or also batch info were added
#' @examples
#'\dontrun{
#'     mirnaCovar(experiment.folder="/home/beccuti/e1g",
#'     covariates=c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.2", "Cov.2", "Cov.2", "Cov.2"),
#'     batches=NULL)
#'     
#'     mirnaCovar(experiment.folder="/Users/raffaelecalogero/Desktop",
#'     covariates=c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.2", "Cov.2", "Cov.2", "Cov.2"),
#'     batches=c("bath.1", "bath.1", "bath.2", "bath.2", "batch.1", "batch.1", "batch.2", "batch.2"))
#' }
#' @export
mirnaCovar <- function(experiment.folders, covariates=NULL, batches=NULL){
       tmp <- read.table(paste(experiment.folder, "all.counts.txt", sep="/"), sep="\t", header=T, stringsAsFactors = F, row.names = 1)   
       if(!is.null(covariates)){
           tmp.n <- names(tmp)
           tmp.n <- sapply(strsplit(tmp.n, "_"),function(x)x[1])
           if(length(tmp.n)==length(covariates)){
                tmp.n <- paste(tmp.n,covariates, sep="_")
           }else{
             cat("\nERROR: the covariates and the samples names do not have the same length\n")
             return(1)
           }
       }
       if(!is.null(batches)){
         if(length(tmp.n)==length(batches)){
           tmp.n <- paste(tmp.n,batches, sep="_")
         }else{
           cat("\nERROR: the batches and the samples names do not have the same length\n")
           return(2)
         }
       }
       if(!is.null(covariates) & !is.null(batches)){
            names(tmp) <- tmp.n
            write.table(tmp, "w_covar_batch_all.counts.txt", sep="\t", col.names = NA, quote = F)
       }else if(!is.null(covariates) & is.null(batches)){
         names(tmp) <- tmp.n
         write.table(tmp, "w_covar_all.counts.txt", sep="\t", col.names = NA, quote = F)
       }
       return(0)
}




