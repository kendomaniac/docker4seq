% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fasterq.dump.R
\name{sraDownload}
\alias{sraDownload}
\title{A function to handle fasterq-dumper SRA to download SRA fastq files}
\usage{
sraDownload(
  group = c("sudo", "docker"),
  sra.name,
  data.folder,
  scratch.folder,
  threads = 8
)
}
\arguments{
\item{group, }{a character string. Two options: sudo or docker, depending to which group the user belongs}

\item{sra.name, }{a character string indicating the name of the SRA object to be download}

\item{data.folder, }{a character string indicating the working folder where output folder will be written}

\item{scratch.folder, }{a character string indicating the temporary folder for data preprocessing}

\item{threads, }{a integer indicating the number of threads to be used from fasterq-dumper}
}
\description{
This function executes a ubuntu docker that produces as output FASTQCstdin_fastqc.html and stdin_fastqc.zip files
}
\examples{
\dontrun{
    #running sraDownload
    sraDownload(group="docker", sra.name="SRR7762358", data.folder=getwd(), scratch.folder="/data/scratch", threads=8)
    system("mv ./SRR7762358/SRR7762358.fastq.gz ./SRR7762358/SRR7762358_S1_L001_R1_001.fastq.gz")
}

}
\author{
Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino
}
