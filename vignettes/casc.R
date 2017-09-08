## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  downloadContainers(group="docker")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#  system("gzip -d singlecells_counts.txt.gz")
#  filterZeros(data.folder=getwd(),counts.matrix="singlecells_counts.txt")
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#  system("gzip -d singlecells_counts.txt.gz")
#  conditions=rep(1,288)
#  checkCountDepth(group="docker", data.folder=getwd(), counts.matrix="singlecells_counts.txt", conditions=conditions, outputName="singlecells_counts", nCores=8)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#  system("gzip -d singlecells_counts.txt.gz")
#  conditions=rep(1,288)
#  scnorm(group="docker", data.folder=getwd(),counts.matrix="singlecells_counts.txt", conditions=conditions,outputName="singlecells_counts", nCores=8, filtercellNum=10, PropToUse=0.1)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#  system("gzip -d singlecells_counts.txt.gz")
#  cascImpute(group="docker", data.folder=getwd(), counts.matrix="singlecells_counts.txt", drop.thre=0.5, cores=8)
#  
#  #Modifying drop.thre value A quick version of the imputing can be used to refine drop.thre values indicating refining=TRUE. It has to be done in the same folder where the frst run was done.
#  cascImpute(group="docker", data.folder=getwd(), counts.matrix="singlecells_counts.txt", drop.thre=0.3, cores=8, refining=TRUE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/singlecells_counts.txt.gz")
#  system("gzip -d singlecells_counts.txt.gz")
#  counts2log(counts.matrix="singlecells_counts.txt", data.folder=getwd(), log.base=10, type="txt")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #downloading fastq files
#  system("wget http://130.192.119.59/public/log10_singlecells_counts.csv.gz")
#  system("gzip -d log10_singlecells_counts.csv.gz")
#  
#  cascKoptimization(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", data.folder=getwd(),
#  counts.matrix="log10_singlecells_counts.csv", permutations=20, blocks.permutations=2, core=0, bootstrap.fraction=10, k.min=2, k.max=4, totIdentity=80, clusterIdentity=80)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#   #downloading fastq files
#  system("wget http://130.192.119.59/public/example.zip")
#  unzip("example.zip")
#  setwd("./example")
#  cascOutputReformat(data.folder=getwd())

