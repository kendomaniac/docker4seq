## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.pos = 'h')

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(docker4seq)
#  downloadContainers(group="docker")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  downloadContainers(group="docker", containers.file="my_containers.txt")
#  #an example of the my_containers.txt file content
#  docker.io/rcaloger/bwa.2017.01
#  docker.io/rcaloger/chipseq.2017.01
#  docker.io/rcaloger/r340.2017.01

## ----fig.1, fig.cap="rnaseqCounts overall performance", echo=FALSE, eval=TRUE, out.width="70%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mrna_performance_bis.jpg')


## ----fig.2, fig.cap="mirnaCounts overall performance", echo=FALSE, eval=TRUE, out.width="70%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mirnaseq_performance_bis.jpg')


## ----fig.3, fig.cap="chipseqCounts overall performance", echo=FALSE, eval=TRUE, out.width="70%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/chipseq_performance_bis.jpg')


## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  demultiplexing(group="docker",
#        data.folder="/home/calogero/Documents/data/lollini/3a_run/170712_NB501050_0097_AH3FGNBGX3",
#        scratch.folder="/data/scratch", threads=24)

## ----fig.4, fig.cap="mRNAseq workflow", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/rnaseq1.jpeg')


## ----fig.5, fig.cap="Creating a STAR genome index", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/rnaseq2.jpeg')


## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  rsemstarIndex(group="docker",genome.folder="/data/scratch/hg38star",
#  ensembl.urlgenome="ftp://ftp.ensembl.org/pub/release-87/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.toplevel.fa.gz",
#  ensembl.urlgtf="ftp://ftp.ensembl.org/pub/release-87/gtf/homo_sapiens/Homo_sapiens.GRCh38.87.gtf.gz")

## ----fig.6, fig.cap="Gene, Isoform counting", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/rnaseq3.jpeg')


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

## ----fig.7, fig.cap="Salmon reference", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/salmonIndex.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  
#  #running salmonIndex human
#  salmonIndex(group="docker", index.folder=getwd(),
#         ensembl.urltranscriptome="ftp://ftp.ensembl.org/pub/release-90/fasta/homo_sapiens/cdna/Homo_sapiens.GRCh38.cdna.all.fa.gz",
#         ensembl.urlgtf="ftp://ftp.ensembl.org/pub/release-90/gtf/homo_sapiens/Homo_sapiens.GRCh38.90.gtf.gz",
#         k=31)
#  

## ----fig.8, fig.cap="Salmon quantification", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/salmonCounts.jpeg')


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

## ----fig.9, fig.cap="RSEM versus SALMON", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/salmon_rsem.jpg')


## ----fig.10, fig.cap="gtf_annotated_genes.results", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/rnaseq7.jpeg')


## ----fig.11, fig.cap="counts table with covariates", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/counts1.jpeg')


## ----fig.12, fig.cap="counts table with covariates and batch", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/counts2.jpeg')


## ----fig.13, fig.cap="generating a table with covariates", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/counts3.jpeg')


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

## ----fig.14, fig.cap="PCA", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/pca1.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  pca(experiment.table="_log2FPKM.txt", type="FPKM", legend.position="topleft", covariatesInNames=FALSE, principal.components=c(1,2), pdf = TRUE, output.folder=getwd())
#  

## ----fig.15, fig.cap="pca.pdf", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/pca2.jpeg')


## ----fig.16, fig.cap="sample size estimation", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/ss.jpeg')


## ----fig.17, fig.cap="experiment power estimation", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/es.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  sampleSize(group="docker", filename="_counts.txt", power=0.80, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#  

## ----fig.18, fig.cap="sample_size_evaluation.txt", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/samplesize1.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  experimentPower(group="docker", filename="_counts.txt",replicatesXgroup=7, FDR=0.1, genes4dispersion=200, log2fold.change=1)
#  

## ----fig.19, fig.cap="power_estimation.txt", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/expp1.jpeg')


## ----fig.20, fig.cap="DESeq2", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/de1.jpeg')


## ----fig.21, fig.cap="DEfull.txt", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/de2.jpeg')


## ----fig.22, fig.cap="DEfiltered_log2fc_1_fdr_0.1.txt", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/de3.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#  wrapperDeseq2(output.folder=getwd(), group="docker",
#        experiment.table="_counts.txt", log2fc=1, fdr=0.1,
#        ref.covar="Cov.1", type="gene", batch=FALSE)

## ----fig.23, fig.cap="Count Filter", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/filtercounts.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  system("wget 130.192.119.59/public/test.analysis.zip")
#  unzip("test.analysis.zip")
#  setwd("test.analysis")
#  library(docker4seq)
#       wrapperDeseq2(output.folder=getwd(), group="docker", experiment.table="_counts.txt", log2fc=1,
#       fdr=0.1, ref.covar="Cov.1", type="gene", batch=FALSE))
#  
#      filterCounts(data.folder=getwd(), type="gene")
#  

## ----fig.24, fig.cap="miRNAseq workflow", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mirna1.jpeg')


## ----fig.25, fig.cap="miRNAseq workflow", echo=FALSE, eval=TRUE, out.width="50%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mirna3.jpeg')


## ----fig.26, fig.cap="miRNAseq parameters", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mirna2.jpeg')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  #test example
#  system("wget 130.192.119.59/public/test.mirnaCounts.zip")
#  unzip("test.mirnaCounts.zip")
#  setwd("test.mirnaCounts")
#  library(docker4seq)
#  mirnaCounts(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#              mirbase.id="hsa",download.status=FALSE, adapter.type="NEB", trimmed.fastq=FALSE)
#  

## ----fig.27, fig.cap="miRNAseq covariates and batches", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/mirna_covars.jpeg')


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

## ----fig.28, fig.cap="ChIPseq workflow", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/chipseq0.jpeg')


## ----fig.29, fig.cap="Creating a BWA index with Genome indexing BWA", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/chipseq1.jpeg')


## ----  echo=TRUE, eval=FALSE---------------------------------------------
#  bwaIndexUcsc(group="sudo",genome.folder="/sto2/data/scratch/mm10bwa", uscs.urlgenome=
#  "http://hgdownload.cse.ucsc.edu/goldenPath/mm10/bigZips/chromFa.tar.gz",
#  gatk=FALSE)

## ----fig.30, fig.cap="MACS and SICER analysis", echo=FALSE, eval=TRUE, out.width="100%", fig.align="center"----
library(knitr)

include_graphics('../inst/img/chipseq3.jpeg')


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

