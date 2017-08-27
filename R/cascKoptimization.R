#' @title Running CASC step1 identification of the optimal number of clusters
#' @description This function executes CASC step1 identification of the optimal number of clusters
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where comma separated file of cells log10 counts is saved
#' @param counts.matrix, a character string indicating the the name of csv  file of cells log10 counts
#' @param permutations, a integer indicating how main permutations of SIMLR are going to be executed.
#' @param blocks.permutations, a integer indicating how many SIMLR permutation are going to be run in parallel. e.g. 10 indicates that 100 permutatin will be run in 10 groups of 10 permutations.
#' @param core, a integer between 0 to 1 to define the fraction of cores to be used by SIMLR, default 0, other values will generate an error in specific hadrware configurations.
#' @param bootstrap.fraction, a integer between 1 and 100, which indicate the fraction of cell to be removed during boostrap.
#' @param k.min, min number of clusters.
#' @param k.max, min number of clusters.
#' @param totIdentity, an integer between 1 to 100 which indicates the percentage of identity required to consider the results of two permutations identical.
#' @param clusterIdentity, an integer between 1 to 100 which indicates the percentage of identity required to consider two clusters identical.
#' @author Raffaele Calogero, Luca Alessandri
#' 
#' @return ...
#' @examples
#' \dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/log10_singlecells_counts.csv.gz")
#'     system("gzip -d log10_singlecells_counts.csv.gz")
#'     #running cascKoptimization
#'     cascKoptimization(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#'     counts.matrix="log10_singlecells_counts.csv", permutations=20, blocks.permutations=2, core=0, bootstrap.fraction=10, k.min=2, k.max=4, totIdentity=80, clusterIdentity=80)
#' }
#'
#' @export
cascKoptimization <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), counts.matrix, permutations=20, blocks.permutations=2, core=0, bootstrap.fraction=10, k.min, k.max,  totIdentity=80, clusterIdentity=80){

  cat("\ncascStep1 start \n)")
  cascStep1(group=group, scratch.folder=scratch.folder, data.folder=data.folder, counts.matrix=counts.matrix, permutations=permutations, blocks.permutations=blocks.permutations, core=core, bootstrap.fraction=bootstrap.fraction, k.min=k.min, k.max=k.max)
  cat("\ncascStep1 end \n)")
  cat("\ncascStep2\n)")
  cascStep2(group=group, scratch.folder=scratch.folder, data.folder=data.folder, totIdentity=totIdentity, clusterIdentity=clusterIdentity, k.min=k.min, k.max=k.max, counts.matrix=counts.matrix)
  cat("\ncascStep2 end \n)")
}
