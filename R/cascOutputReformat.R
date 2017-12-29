#' @title Organizing in a unique table for each K the output of cascKoptimization function
#' @description This function converts organize in a table each output of cascKoptimization function
#' @param data.folder, a character string indicating the cascKoptimization output folder containing K folders, SilhouetteValue.csv, cell.stability.pdf and silhouette.pdf
#' @return Returns in each K folder a tab delimited file called summary_table.txt, including the SilhouetteParameters (Silhouette score for each cell: extraScore, intraScore, ClusterBelong, Neighbor, SilhouetteValue), SIMLR.cluster, cell.stability.score, SIMLR component 1, SIMLR component 2.
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/example.zip")
#'     unzip("example.zip")
#'     setwd("./example")
#'     cascOutputReformat(data.folder=getwd())
#'
#' }
#' @export

cascOutputReformat <- function(data.folder){
   home <- getwd()
   setwd(data.folder)
   k.dir <- list.dirs(getwd(), recursive = FALSE)
   for(i in k.dir){
          setwd(i)
          sil <- read.table("SilhouetteParameters.csv", sep=",", header=T, stringsAsFactors=F)
          mainVector <- read.table("mainVector.csv", sep=",", header=T, stringsAsFactors=F)
          scoreVector <- read.table("scoreVector.csv", sep=",", header=T, stringsAsFactors=F)
          dataPlot <- read.table("dataPlot.csv", sep=",", header=T, stringsAsFactors=F)
          matrixWithScore <- read.table("MatrixWithScore.csv", sep=",", header=T, stringsAsFactors=F, row.names=1)
          tmp <- strsplit(names(matrixWithScore),'\\.')
          cellID <- sapply(tmp, function(x)x[1])
          summary.table <- data.frame(cellID, sil[,2:6], mainVector[,2], scoreVector[,2],dataPlot[,2:3])
          names(summary.table) <- c("cellID", "extraScore", "intraScore", "ClusterBelong",
                                    "Neighbor", "SilhouetteValue","SIMLR.cluster","cell.stability.score",
                                    "SIMLR component 1", "SIMLR component 2")
          write.table(summary.table, "summary_table.csv", sep=",", row.names=F)
          setwd(data.folder)
   }
}

