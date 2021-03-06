% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapperCiri.R
\name{wrapperCiri}
\alias{wrapperCiri}
\title{Wrapper function for circRNAs prediction using CIRI 2}
\usage{
wrapperCiri(
  group = c("sudo", "docker"),
  scratch.folder,
  data.folder,
  genome.file,
  seq.type = c("se", "pe"),
  sample.id,
  threads = 1,
  annotation.file = "",
  max.span = 2e+05,
  stringency.value = c("high", "low", "zero"),
  quality.threshold = 10
)
}
\arguments{
\item{group, }{a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs}

\item{scratch.folder, }{a character string indicating the scratch folder where docker container will be mounted}

\item{data.folder, }{a character string indicating where gzip fastq files are located}

\item{genome.file, }{a character string indicating the path to the Fasta file of the reference genomic sequence (it should be the same reference indexed for the BWA alignment)}

\item{seq.type, }{a character string indicating the type of reads to be trimmed. Two options: \code{"se"} or \code{"pe"} respectively for single end and pair end sequencing}

\item{sample.id, }{a character string indicating the unique id to be associated to the bam that will be created}

\item{threads, }{a number indicating the number of cores to be used from the application}

\item{annotation.file, }{a character string indicating the path to the GTF/GFF file reporting the reference gene annotations}

\item{max.span, }{an integer reporting the maximum spanning distance of a circRNA (default = 200000 bp)}

\item{stringency.value, }{the selected stringency level of the analysis. Three possible options are available: "high" (high stringency, default), in which CIRI2 only provides circRNAs supported by more than 2 distinct PCC signals; "low" (low stringency), CIRI2 only provides circRNAs supported by more than 2 junction reads; "zero", CIRI2 provides all circRNAs regardless junction read counts or PCC signals}

\item{quality.threshold, }{integer indicating the threshold for mapping quality of each segment of junction reads (default=10)}
}
\value{
The list of circRNAs predicted by CIRI starting from the raw RNA-Seq datasets
}
\description{
This function calls sequentially the docker containers for FASTQC, BWA, and CIRI to predict the list of circRNAs starting from the raw RNA-Seq reads
}
\examples{
\dontrun{

    #retrieve the example data
    system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #retrieve the data of the indexed genome (chromosome 21 of hg38 human genome assembly)
    system("unzip master.zip")
    system("unzip ./circhunter-master/CircHunter/data/hg38.chr21.fa.zip")
    system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_1.fastq.gz") #retrieve the RNA-Seq data
    system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_2.fastq.gz") #retrieve the RNA-Seq data

    #running the wrapperCiri function
wrapperCiri(group = "docker", scratch.folder="/data/scratch", data.folder=getwd(), genome.file="./circhunter-master/CircHunter/data/hg38.chr21.fa", seq.type = "pe", sample.id="test", threads = 1, max.span = 200000, stringency.value = "high", quality.threshold = 10) 

}
}
\author{
Nicola Licheri and Giulio Ferrero
}
