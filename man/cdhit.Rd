% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdhit.R
\name{cdhit}
\alias{cdhit}
\title{A function to execute CD-HIT}
\usage{
cdhit(
  group = c("sudo", "docker"),
  scratch.folder,
  data.folder,
  identity.threshold = 0.9,
  memory.limit = 30000,
  threads = 0,
  word.length = 7
)
}
\arguments{
\item{group, }{a character string. Two options: sudo or docker, depending to which group the user belongs}

\item{scratch.folder, }{a character string indicating the path of the scratch folder}

\item{data.folder, }{a character string indicating the folder where input data are located and where output will be written}

\item{identity.threshold, }{sequence identity threshold, default 0.9, this is the default cd-hit's global sequence identity calculated as: number of identical bases in alignment divided by the full length of the shorter sequence}

\item{memory.limit, }{memory limit in MB for the program, default 30000. 0 for unlimitted}

\item{threads, }{number of threads, default 0; with 0, all CPUs will be used}

\item{word.length, }{7 for thresholds between 0.88 and 0.9 for other option see user manual cdhit}
}
\value{
Returns two files: a fasta file of representative sequences and a text file of list of clusters
}
\description{
This function executes a ubuntu docker that cluster minION sequences using CD-HIT
}
\examples{
\dontrun{
    #running fastq2fasta
    cdhit(group="docker", scratch.folder="/data/scratch", data.folder=getwd(), identity.threshold=0.90, memory.limit=8000, threads=0, word.length=7)
}

}
\author{
Raffaele A Calogero, raffaele.calogero [at] unito [dot] it, University of Torino. Italy
}
