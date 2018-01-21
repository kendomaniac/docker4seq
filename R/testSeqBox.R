
#' @title Testing the SeqBox basic installation
#' @description This function executes a set of scripts to check if SeqBox installation is OK. It requests the existence of the following folders: /data/genomes, /data/scratch, /data/tests. /data/genomes folder must contain the following folders: hg38star  mm10bwa  mm10star
#' @author Raffaele Calogero
#'
#' @return testSeqBox.out file containing the results of the tests
#' @examples
#'\dontrun{
#'     #downloading in /data/genomes
#'     setwd("/data/genomes")
#'     system("wget http://130.192.119.59/public/hg38star.tar.gz")
#'     system("wget http://130.192.119.59/public/mm10star.tar.gz")
#'     system("wget http://130.192.119.59/public/mm10bwa.tar.gz")
#'     system("wget http://130.192.119.59/public/hg38salmon.tar.gz")
#'     system("gzip -d *.gz")
#'     system("tar xvf *.tar")
#'     setwd("/data/")
#'     system("wget http://130.192.119.59/public/tests.tar.gz")
#'     system("gzip -d tests.tar.gz")
#'     system("tar xvf tests.tar")
#'     system("rm tests.tar")
#'     #running test SeqBox
#'     library(docker4seq)
#'     testSeqbox()
#'
#' }
#' @export
testSeqbox <- function(){
     cat("\nInstalling the latest version of docker4seq stable release\n")
     time <- gsub(":","-",gsub(" ","-",date()))
     zz <- file("testSeqBox.out", "w+")
     writeLines(time,con=zz)
     writeLines("Installing the latest version of docker4seq stable release",con=zz)
     library(devtools)
     install_github("kendomaniac/docker4seq", ref="master")
     cat("\nInstalling dockers images\n")
     writeLines("Installing dockers images",con=zz)
     source("/data/tests/downloadContainers/script.R")
     #
     #testing RNAseq workflow
     cat("\ntesting RNAseq workflow\n")
     writeLines("testing RNAseq workflow",con=zz)
     writeLines(time,con=zz)

     cat("\ntesting rsemstarIndex\n")
     setwd("/data/tests/rsemstarIndex/")
     #source("/data/tests/rsemstarIndex/script.R")

     cat("\ntesting rnaseqCounts\n")
     setwd("/data/tests/rnaseqCounts")
     source("/data/tests/rnaseqCounts/script.R")

     #testing salmon reference free aligner
     cat("\ntesting salmonCounts\n")
     setwd("/data/tests/salmonCounts")
     source("/data/tests/salmonCounts/script.R")

     cat("\ntesting samples2experiment\n")
     setwd("/data/tests/samples2experiment")
     source("/data/tests/samples2experiment/script.R")

     cat("\ntesting sampleSize\n")
     setwd("/data/tests/sampleSize")
     source("/data/tests/sampleSize/script.R")

     cat("\ntesting experimentPower\n")
     setwd("/data/tests/experimentPower")
     source("/data/tests/experimentPower/script.R")

     cat("\ntesting pca\n")
     setwd("/data/tests/pca")
     source("/data/tests/pca/script.R")

     cat("\ntesting wrapperDeseq2\n")
     setwd("/data/tests/wrapperDeseq2")
     source("/data/tests/wrapperDeseq2/script.R")
     #testing miRNAseq
     cat("\ntesting miRNAseq workflow\n")
     writeLines("testing miRNAseq",con=zz)
     writeLines(time,con=zz)

     cat("\ntesting mirnaCounts\n")
     setwd("/data/tests/mirnaCounts")
     source("/data/tests/mirnaCounts/script.R")
     #testing chipseq
     cat("\ntesting chipseq workflow\n")
     writeLines("testing chipseq",con=zz)
     writeLines(time,con=zz)

     cat("\ntesting bwaIndexUcsc\n")
     setwd("/data/tests/bwaIndexUcsc/")
     #source(/data/tests/bwaIndexUcsc/script.R

     cat("\ntesting chipseqCounts\n")
     setwd("/data/tests/chipseqCounts")
     source("/data/tests/chipseqCounts/script.R")
     #
     #checking results
     cat("\nchecking results\n")
     writeLines(time,con=zz)
     setwd("/data/tests/rsemstarIndex/")
     #.....TO BE defined
     setwd("/data/tests/rnaseqCounts")
     dir <- dir()
     if(length(grep("trimmed.log$", dir))==0){
       writeLines("Skewer failed",con=zz)
     }else{
       writeLines("skewer passed test",con=zz)
     }
     if(length(grep("Log.final.out", dir))==0){
       writeLines("STAR failed",con=zz)
     }else{
       writeLines("STAR passed test",con=zz)
     }
     if(length(grep("isoforms.results", dir))==0){
       writeLines("RSEM failed",con=zz)
     }else{
       writeLines("RSEM passed tests checks",con=zz)
     }
     if(length(grep("gtf_annotated_genes.results", dir))==0){
       writeLines("annotation by GTF failed",con=zz)
     }else{
       writeLines("annotation by GTF passed test",con=zz)
     }
     #
     setwd("/data/tests/salmonCounts")
     dir <- dir()
     if(length(grep("gtf_annotated_genes.results", dir))==0){
       writeLines("annotation by GTF failed",con=zz)
     }else if(length(grep("isoforms.results", dir))==0){
       writeLines("Salmon to RSEM conversion failed",con=zz)
     }else if(length(grep("quant.sf", dir))==0){
       writeLines("Salmon failed",con=zz)
     }else if(length(grep("trimmed.log$", dir))==0){
       writeLines("Skewer failed",con=zz)
     }else{
       writeLines("salmonCounts passed test",con=zz)
     }
     #
     setwd("/data/tests/samples2experiment")
     dir <- dir()
     if(length(grep("test.samples2experiment", dir))==0){
       writeLines("wget failed",con=zz)
     }
     setwd("/data/tests/samples2experiment/test.samples2experiment")
     dir <- dir()
     if(length(grep("_experiment.tables.Rda", dir))==0){
       writeLines("samples2experiment failed check 1",con=zz)
     }else{
       load(dir[grep("_experiment.tables.Rda", dir)])
     }
     if(dim(counts)[1]>10){
       writeLines("samples2experiment passed test check",con=zz)
     }else{
       writeLines("samples2experiment failed check 2",con=zz)
     }
     #
     setwd("/data/tests/sampleSize")
     dir <- dir()
     if(length(grep("test.analysis", dir))==0){
       writeLines("wget failed",con=zz)
     }
     setwd("/data/tests/sampleSize/test.analysis")
     dir <- dir()
     if(length(grep("sample_size_evaluation.txt", dir))==0){
       writeLines("sampleSize failed",con=zz)
     }else{
       writeLines("sampleSize passed test",con=zz)
     }
     #
     setwd("/data/tests/experimentPower")
     dir <- dir()
     if(length(grep("test.analysis", dir))==0){
       cat("\n\n")
       writeLines("wget failed",con=zz)
     }
     #
     setwd("/data/tests/experimentPower/test.analysis")
     dir <- dir()
     if(length(grep("power_evaluation.txt", dir))==0){
       writeLines("experimentPower failed test",con=zz)
     }else{
       writeLines("experimentPower passed test",con=zz)
     }
     #
     #testing miRNAseq
     setwd("/data/tests/mirnaCounts")
     dir <- dir()
     if(length(grep("test.mirnaCounts", dir))==0){
       writeLines("wget failed",con=zz)
     }
     #
     setwd("/data/tests/mirnaCounts/test.mirnaCounts")
     dir <- dir()
     if(length(grep("all_counts.Rda", dir))==0){
       writeLines("mirnaCounts failed test check 1",con=zz)
     }else{
       load(dir[grep("all_counts.Rda", dir)])
     }
     if(dim(all.counts)[1]>10){
       writeLines("mirnaCounts passed test",con=zz)
     }else{
       writeLines("mirnaCounts failed test",con=zz)
     }
     #
     #testing chipseq
     setwd("/data/tests/bwaIndexUcsc/")
     #....TO BE DEFINED
     setwd("/data/tests/chipseqCounts")
     dir <- dir()
     if(length(grep("test.chipseqCounts", dir))==0){
       writeLines("wget failed",con=zz)
     }
     #
     setwd("/data/tests/chipseqCounts/test.chipseqCounts/prdm51.igg")
     dir <- dir()
     if(length(dir)>=23){
       writeLines("chipseqCounts passed test",con=zz)
     }else{
       writeLines("chipseqCounts failed test",con=zz)
     }
     time <- gsub(":","-",gsub(" ","-",date()))
     writeLines(time,con=zz)
     close(zz)
     cat("\nTests are finished, please check testSeqBox.out for results\n")

}




