#' @title CASC requires log10 counts table in comma delimited file
#' @description This function converts raw counts table in CASC suitable table.
#' @param counts.matrix, a character string indicating the the name of txt or csv  file of cells counts
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts will be saved
#' @param type, a character value indicating the content of the file separator: csv comma, txt tab
#' @param log.base, a integer value indicating the base fro the log transformation
#' @return Returns a log10 or log2 comma separated file.
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#'     system("gzip -d singlecells_counts.txt.gz")
#'     counts2log(counts.matrix="singlecells_counts.txt", data.folder=getwd(), log.base=10, type="txt")
#'
#' }
#' @export

counts2log <- function(counts.matrix, data.folder=getwd(), log.base=c(2,10), type=c("txt","csv")){
  if(type=="txt"){
       tmp <- read.table(counts.matrix, sep="\t", stringsAsFactors = TRUE, header=T, check.names = FALSE, row.names=1)
       if(log.base==2){
           tmpl <- log2(tmp +1)
           write.table(tmpl, paste("log2_",sub(".txt","", counts.matrix), ".csv", sep=""), sep=",", col.names=NA)
       }else if(log.base==10){
           tmpl <- log10(tmp +1)
           write.table(tmpl, paste("log10_",sub(".txt","", counts.matrix), ".csv", sep=""), sep=",", col.names=NA)
       }
   }else if(type=="csv"){
      tmp <- read.table(counts.matrix, sep=",", stringsAsFactors = TRUE, header=T, check.names = FALSE, row.names=1)
      if(log.base==2){
         tmpl <- log2(tmp +1)
         write.table(tmpl, paste("log2_",sub(".csv","", counts.matrix), ".csv", sep=""), sep=",", col.names=NA)
      }else if(log.base==10){
         tmpl <- log10(tmp +1)
         write.table(tmpl, paste("log10_",sub(".csv","", counts.matrix), ".csv", sep=""), sep=",", col.names=NA)
      }
   }
}

