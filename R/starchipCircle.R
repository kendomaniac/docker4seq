#' @title Running starchip to detect circular RNAs on paired-end sequences
#' @description This function execute starchip on a set of folders containing the output of starChimeric. It requires a specific bed generated with starChipIndex in the genome folder used by starChimeric
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param scratch.folder,  a character string indicating the scratch folder where docker container will be mounted
#' @param reads.cutoff, Integer. Minimum number of reads crossing the circRNA backsplice required.
#' @param min.subject.limit, Integer. Minimum number of individuals with readsCutoff reads required to carry forward a circRNA for analysis
#' @param threads, Integer. Number of threads to use
#' @param do.splice, true false. The splices within the circRNA be detected and reported. Linear splices are searched within each circRNA in each individual. Any linear splice with >= 60\% of the read count of the cRNA is considered a splice within the circRNA. Two files are then created, .consensus with most common splice pattern, and .allvariants with all reported splice patterns.
#' @param cpm.cutoff, Float. Reads counts are loaded into R and log2(CountsPerMillion) is calculated using the limma package. With cpmCutoff > 0, circRNAs with log2(CPM) below this value will be filtered from this analysis
#' @param subjectCPM.cutoff, Integer. See above. This value is the lower limit for number of individuals required to have the circRNAs expressed at a value higher than cpmCutoff.
#' @param annotation, true/false. circRNAs are provided with gene annotations
#' @param samples.folder, the folder where are located all the folders of the samples processed with starChimeric
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return 1. Count matrices : raw cRNA backsplice counts: circRNA.cutoff[readthreshold]reads.[subjectthreshold]ind.countmatrix log2CPM of above: norm_log2_counts_circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples.txt Maximum Linear Splices at Circular Loci: rawdata/linear.[readthreshold]reads.[subjectthreshold]ind.sjmax 2. Info about each circRNA:  Consensus Information about Internal Splicing: Circs[reads].[subjects].spliced.consensus Complete Gene Annotation: circRNA.[readthreshold]reads.[subjectthreshold]ind.annotated Consise Gene Annotation + Splice Type:  circRNA.[readthreshold]reads.[subjectthreshold]ind.genes 3. Images: PCA plots: circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples_variance_PCA.pdf Heatmap: circRNA.[readthreshold]reads.[subjectthreshold]ind.heatmap.pdf
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     starchipCircle(group="docker", genome.folder="/data/genomes/hg38star", scratch.folder="/data/scratch",
#'                        samples.folder=getwd(), reads.cutoff=1, min.subject.limit=2, threads=8,
#'                        do.splice = TRUE, cpm.cutoff=0, subjectCPM.cutoff=0, annotation=TRUE)
#' }
#'
#' @export

starchipCircle <- function(group=c("sudo","docker"), scratch.folder, genome.folder,
                           samples.folder, reads.cutoff, min.subject.limit, threads,
                           do.splice = c(TRUE, FALSE), cpm.cutoff=0,
                           subjectCPM.cutoff=0, annotation=c(TRUE, FALSE)) {

  #running time 1
  ptm <- proc.time()

  scratch.folder <- normalizePath(scratch.folder)
  genome.folder <- normalizePath(genome.folder)
  samples.folder <- normalizePath(samples.folder)

  #obtaining output data folder
  data.folder <- samples.folder

  #setting the data.folder as working folder
  if (!file.exists(data.folder)) {
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }

  #storing the position of the home folder
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")


  #check  if scratch folder exist
  if (!file.exists(scratch.folder)) {
    cat(paste("\nIt seems that the", scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }

  #executing the docker job
  params <- paste(
    "--cidfile", paste0(samples.folder, "/dockerID"),
    "-v", paste0(samples.folder, ":/samples"),
    "-v", paste0(genome.folder, ":/genome"),
    "-v", paste0(scratch.folder, ":/scratch"),
    "-d docker.io/repbioinfo/star251.2019.02 /bin/bash /bin/start_starchipCircle.sh",
    reads.cutoff, min.subject.limit, threads,
    ifelse(do.splice, "True", "False"),
    cpm.cutoff, subjectCPM.cutoff,
    ifelse(annotation, "true", "false")
  )
  resultRun <- runDocker(group=group, params=params)

  if(resultRun == 0) {
    cat("\nThe STARChip analysis is finished\n")
  }

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0) {
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("STARChip user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("STARChip system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("STARChip elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  } else {
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("STARChip system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("STARChip elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/starchipCircle_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  # removing temporary files
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
