% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sncRNA.R
\name{sncRNA}
\alias{sncRNA}
\title{Running small RNA-seq single-end reads alignment and quantification using BWA and custom scripts}
\usage{
sncRNA(
  group = c("sudo", "docker"),
  fastq.folder = getwd(),
  scratch.folder,
  mode,
  reference,
  threads = 1,
  mb.url.haripin = NULL,
  mb.url.mature = NULL,
  mb.species = NULL,
  adapter.type = c("NEB", "ILLUMINA", "QIAGEN", "LEXOGEN", "DIAGENE", "SEQMATIC",
    "TRILINK"),
  trimmed.fastq = FALSE
)
}
\arguments{
\item{group, }{a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs}

\item{fastq.folder, }{a character string indicating where trimmed fastq files are located}

\item{scratch.folder, }{a character string indicating the scratch folder where docker container will be mounted}

\item{mode, }{a character string indicating the required type of analysis. Compatible analyses mode are "miRNA" and "ncRNA". In "miRNA" analysis mode, the version ("mb.version" argument) and species prefix ("mb.species" argument) of miRBase are required. This mode require also the "reference" argument. In the "ncRNA" mode only the "reference" argument is required.}

\item{reference, }{a character string indicating the path to the reference fasta file used to create the BWA index}

\item{threads, }{a number indicating the number of cores to be used from the application}

\item{mb.url.haripin, }{character string indicating the link to the hairpin miRNA sequences miRBase database. Visit http://www.mirbase.org to select the proper version number.}

\item{mb.url.mature, }{a character string indicating the link to the mature miRNA sequences from miRBase database. Visit http://www.mirbase.org to select the proper version number.}

\item{mb.species, }{a character string indicating the three-letter prefix of a species annotated in miRBase (e.g. "hsa" for human miRNAs). Please refer to http://www.mirbase.org/help/genome_summary.shtml to obtain the proper species prefix.}

\item{adapter.type, }{a character string. Seven options are available depending on which miRNA library prep was used: NEB, ILLUMINA, QIAGEN, LEXOGEN, DIAGENE, SEQMATIC, TRILINK}

\item{trimmed.fastq, }{a boolean logical variable indicating if trimmed fastq are saved. Default is FALSE}
}
\value{
Read count table of RNA-Seq reads aligned miRNA or non-miRNA annotations
}
\description{
This function executes the docker container where BWA is installed. BWA is a read alignment package that efficiently align short sequencing reads against a large reference sequence. Alignment is performed against annotations of human small RNAs. Read count is performed by GenomicAlignments R package and custom Python and bash commands.
}
\examples{
\dontrun{
    #downloading fastq files
    system("wget http://130.192.119.59/public/test_R1.fastq.gz")

    #running miRNAs quantification pipeline
    bwaIndex(group="docker", genome.folder="/data/genomes", mb.url.hairpin=https://www.mirbase.org/download/hairpin.fa, mb.url.mature=https://www.mirbase.org/download/mature.fa, mb.species="hsa", mode="miRNA")
    sncRNA(group="docker", fastq.folder=getwd(), scratch.folder="/data/scratch", mode="miRNA", reference="/data/genome/hairpin_hsa_miRBase.fa", mb.url.hairpin=https://www.mirbase.org/download/hairpin.fa, mb.url.mature=https://www.mirbase.org/download/mature.fa, threads=8, , mb.species="hsa")

    #running non miRNA ncRNAs quantification pipeline
    bwaIndex(group="docker", genome.folder="/data/genomes/", rc.version="9", rc.species="Homo sapiens", length=80, mode="ncRNA")
    sncRNA(group="docker", fastq.folder=getwd(), scratch.folder="/data/scratch", mode="ncRNA", reference="/data/genome/ncRNA_Homo_sapiens_RNA_Central_9.0_len_80.fa", threads=8)

}
}
\author{
Giulio Ferrero
}
