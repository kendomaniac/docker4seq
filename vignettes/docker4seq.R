## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(docker4seq)
#  downloadContainers(group="docker")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  downloadContainers(group="docker", containers.file="my_containers.txt")
#  #an example of the my_containers.txt file content
#  docker.io/rcaloger/bwa.2017.01
#  docker.io/rcaloger/chipseq.2017.01
#  docker.io/rcaloger/r340.2017.01

## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  demultiplexing(group="docker",
#        data.folder="/home/calogero/Documents/data/lollini/3a_run/170712_NB501050_0097_AH3FGNBGX3",
#        scratch.folder="/data/scratch", threads=24)

## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  rsemstarIndex(group="docker",genome.folder="/data/scratch/hg38star",
#  ensembl.urlgenome="ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz",
#  ensembl.urlgtf="ftp://ftp.ensembl.org/pub/release-87/gtf/homo_sapiens/Homo_sapiens.GRCh38.87.gtf.gz")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  
#  #test example
#  system("wget http://130.192.119.59/public/test.mrnaCounts.zip")
#  unzip("test.mrnaCounts.zip")
#  setwd("./test.mrnaCounts")
#  library(docker4seq)
#  rnaseqCounts(group="docker",fastq.folder=getwd(), scratch.folder=getwd(),
#  adapter5="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#  adapter3="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
#  seq.type="se", threads=8,  min.length=40,
#  genome.folder="/data/scratch/mm10star", strandness="none", save.bam=FALSE,
#  org="mm10", annotation.type="gtfENSEMBL")
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  
#  #running salmonIndex human
#  salmonIndex(group="docker", index.folder=getwd(),
#         ensembl.urltranscriptome="ftp://ftp.ensembl.org/pub/release-90/fasta/homo_sapiens/cdna/Homo_sapiens.GRCh38.cdna.all.fa.gz",
#         ensembl.urlgtf="ftp://ftp.ensembl.org/pub/release-90/gtf/homo_sapiens/Homo_sapiens.GRCh38.90.gtf.gz",
#         k=31)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  
#  system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#  system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#  
#  #running salmonCounts
#  wrapperSalmon(group="docker", scratch.folder="/data/scratch/",
#           fastq.folder=getwd(), index.folder="/data/genome/salmonhg38/",
#           threads=8, seq.type="pe", adapter5="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#           adapter3="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT", min.length=40, strandness="none")
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget http://130.192.119.59/public/test.samples2experiment.zip")
#  unzip("test.samples2experiment.zip")
#  setwd("test.samples2experiment")
#  library(docker4seq)
#  sample2experiment(sample.folders=c("./e1g","./e2g","./e3g",
#  "./p1g", "./p2g", "./p3g"),
#  covariates=c("Cov.1","Cov.1","Cov.1","Cov.2","Cov.2","Cov.2"),
#  bio.type="protein_coding", output.prefix=".")
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  pca(experiment.table="_log2FPKM.txt", type="FPKM", legend.position="topleft", covariatesInNames=FALSE, principal.components=c(1,2), pdf = TRUE, output.folder=getwd())
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  sampleSize(group="docker", filename="_counts.txt", power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  experimentPower(group="docker", filename="_counts.txt",replicatesXgroup=7, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  wrapperDeseq2(output.folder=getwd(), group="docker",
#        experiment.table="_counts.txt", log2fc=1, fdr=0.1,
#        ref.covar="Cov.1", type="gene", batch=FALSE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.mirnaCounts.zip")
#  unzip("test.mirnaCounts.zip")
#  setwd("test.mirnaCounts")
#  library(docker4seq)
#  mirnaCounts(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#              mirbase.id="hsa",download.status=FALSE, adapter.type="NEB", trimmed.fastq=FALSE)
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.mirna.analysis.zip")
#  unzip("test.mirna.analysis.zip")
#  setwd("test.mirna.analysis")
#  library(docker4seq)
#  mirnaCovar(experiment.folder=paste(getwd(), "all.counts.txt", sep="/"),
#       covariates=c("Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.1", "Cov.1",
#                    "Cov.2", "Cov.2", "Cov.2", "Cov.2", "Cov.2", "Cov.2"),
#       batches=c("bath.1", "bath.1", "bath.2", "bath.2", "batch.1", "batch.1",
#                 "batch.2", "batch.2","batch.1", "batch.1","bath.2", "bath.2"), output.folder=getwd())

## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  bwaIndexUcsc(group="sudo",genome.folder="/sto2/data/scratch/mm10bwa", uscs.urlgenome=
#  "http://hgdownload.cse.ucsc.edu/goldenPath/mm10/bigZips/chromFa.tar.gz",
#  gatk=FALSE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  
#  system("wget 130.192.119.59/public/test.chipseqCounts.zip")
#  unzip("test.chipseqCounts.zip")
#  setwd("test.chipseqCounts")
#  library(docker4seq)
#  chipseqCounts(group = "docker", output.folder = "./prdm51.igg",
#    mock.folder="./igg", test.folder="./prdm51", scratch.folder=getwd(),
#    adapter5 = "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#    adapter3 = "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
#    threads = 8, min.length = 30, genome.folder,
#    mock.id = "igg", test.id = "tf", genome, read.size = 50,
#    tool = "macs", macs.min.mfold = 10, macs.max.mfold = 30,
#    macs.pval = "1e-5", sicer.wsize = 200, sicer.gsize = 200,
#    sicer.fdr = 0.1, tss.distance = 0, max.upstream.distance = 10000,
#    remove.duplicates = "N")

## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  xenomeIndex(group="docker",xenome.folder="/data/scratch/hg19.mm10",
#      hg.urlgenome="http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz",
#      mm.urlgenome="http://hgdownload.cse.ucsc.edu/goldenPath/mm10/bigZips/chromFa.tar.gz", threads=8)

## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  system("wget http://130.192.119.59/public/mcf7_mouse_1m_R1.fastq.gz")
#  system("wget http://130.192.119.59/public/mcf7_mouse_1m_R2.fastq.gz")
#  #running xenome
#  xenome(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#           xenome.folder="/data/scratch/hg19.mm10", seq.type="pe",
#           threads=8)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  system("wget http://130.192.119.59/public/hg19_exome.tar.gz)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #example set made of 1 million reads of MCF7 exome data and 1 million reads of mouse genomic DNA pulled down with Illumina Nextera Rapid Capture Exome kit.
#  
#  system("wget http://130.192.119.59/public/mcf7_mouse_1m_R1.fastq.gz")
#  system("wget http://130.192.119.59/public/mcf7_mouse_1m_R2.fastq.gz")
#  
#  #running wrapperPdx
#  wrapperPdx(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#       xenome.folder="/data/scratch/hg19.mm10", seq.type="pe", threads=24,
#       adapter5="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
#       adapter3="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT"
#       min.length=40, genome.folder="/data/scratch/hg19_exome", sample.id="sampleX")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  system("wget http://130.192.119.59/public/test_oncosnp.zip")
#  system("unzip test_oncosnp.zip")
#  
#  oncosnpAnnotation(group="docker", data.folder="./test_oncosnp/oncosnp_out", genome.folder="./test_oncosnp/hg19")

