#' @title Running starchip to detect circular RNAs on paired-end sequences
#' @description This function execute starchip on a set of folders containing the output of starChimeric. It requires a specific bed generated with starChipIndex in teh genome folder used by starChimeric
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param scratch.folder,  a character string indicating the scratch folder where docker container will be mounted
#' @param reads.cutoff, Integer. Minimum number of reads crossing the circular RNA backsplice required.
#' @param min.subject.limit, Integer. Minimum number of individuals with readsCutoff reads required to carry forward a cRNA for analysis
#' @param threads, Integer. Number of threads to use
#' @param do.splice, true false. The splices within the cRNA be detected and reported. Linear splices are searched within each cRNA in each individual. Any linear splice with >= 60\% of the read count of the cRNA is considered a splice within the cRNA. Two files are then created, .consensus with most common splice pattern, and .allvariants with all reported splice patterns.
#' @param cpm.cutoff, Float. Reads counts are loaded into R and log2(CountsPerMillion) is calculated using the limma package. With cpmCutoff > 0, cRNA with log2(CPM) below this value will be filtered from this analysis
#' @param subjectCPM.cutoff, Integer. See above. This value is the lower limit for number of individuals required to have the cRNA expressed at a value higher than cpmCutoff.
#' @param annotation, rue/false. cRNA are provided with gene annotations
#' @param samples.folder, the folder where are located all the folders of the samples processed with starChimeric
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return 1. Count matrixes : raw cRNA backsplice counts: circRNA.cutoff[readthreshold]reads.[subjectthreshold]ind.countmatrix log2CPM of above: norm_log2_counts_circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples.txt Maximum Linear Splices at Circular Loci: rawdata/linear.[readthreshold]reads.[subjectthreshold]ind.sjmax 2. Info about each circRNA:  Consensus Information about Internal Splicing: Circs[reads].[subjects].spliced.consensus Complete Gene Annotation: circRNA.[readthreshold]reads.[subjectthreshold]ind.annotated Consise Gene Annotation + Splice Type:  circRNA.[readthreshold]reads.[subjectthreshold]ind.genes 3. Images: PCA plots: circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples_variance_PCA.pdf Heatmap: circRNA.[readthreshold]reads.[subjectthreshold]ind.heatmap.pdf
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     starchipCircle(group="docker", genome.folder="/data/genomes/hg38star", scratch.folder="/data/scratch",
#'                        samples.folder=getwd(), reads.cutoff=1, min.subject.limit=2, threads=8,
#'                        do.splice = "True", cpm.cutoff=0, subjectCPM.cutoff=0, annotation="true")
#' }
#'
#' @export
starchipCircle <- function(group=c("sudo","docker"), genome.folder, scratch.folder, samples.folder,
                           reads.cutoff, min.subject.limit, threads,
                           do.splice = c("True", "False"),cpm.cutoff=0,
                           subjectCPM.cutoff=0, annotation=c("true", "false")){

  home <- getwd()

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(samples.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(scrat_tmp.folder)
  dir.create(paste(scrat_tmp.folder,"/samples", sep=""))
  dir <- dir(path=samples.folder)
  dir.info <- dir[which(dir=="run.info")]
  if(length(dir.info)>0){
    system(paste("chmod 777 -R", scrat_tmp.folder))
    system(paste("cp ",samples.folder,"/run.info ", scrat_tmp.folder,"/samples/run.info", sep=""))

  }

  setwd(samples.folder)
  dir <- list.dirs(recursive = FALSE)
  dir <- sub("\\./","/samples/", dir)

  zz <- file("STARdirs.txt", "w")
  writeLines(dir, zz)
  close(zz)

  zz <- file(paste(scrat_tmp.folder,"/samples/STARdirs.txt", sep=""), "w")
  writeLines(dir, zz)
  close(zz)

  for(i in dir){
    dir.create(paste(scrat_tmp.folder, i, sep=""))
    system(paste("cp ", sub("/samples/","",i),"/Chimeric.junction.out ", scrat_tmp.folder, i, sep=""))
  }

  params.file=paste(path.package(package="docker4seq"),"extras/starchip-circles.params",sep="/")
  system(paste("cp ",params.file," ", samples.folder, "/Parameters.txt",sep=""))

  #edit params file
  pf <- readLines("Parameters.txt")
  readsCutoff <- pf[grep("readsCutoff", pf)]
  readsCutoff <- sub("5", reads.cutoff, readsCutoff)
  pf[grep("readsCutoff", pf)] <- readsCutoff

  minSubjectLimit <- pf[grep("minSubjectLimit", pf)]
  minSubjectLimit <- sub("10", min.subject.limit, minSubjectLimit)
  pf[grep("minSubjectLimit", pf)] <- minSubjectLimit

  cpus <- pf[grep("cpus", pf)]
  cpus <- sub("8", threads, cpus)
  pf[grep("cpus", pf)] <- cpus

  do_splice <- pf[grep("do_splice", pf)]
  do_splice <- sub("True", do.splice, do_splice)
  pf[grep("do_splice", pf)] <- do_splice

  cpmCutoff <- pf[grep("cpmCutoff", pf)]
  cpmCutoff <- sub("0", cpm.cutoff, cpmCutoff)
  pf[grep("cpmCutoff", pf)] <- cpmCutoff

  subjectCPMcutoff <- pf[grep("subjectCPMcutoff", pf)]
  subjectCPMcutofff <- sub("0", subjectCPM.cutoff, subjectCPMcutoff)
  pf[grep("subjectCPMcutoff", pf)] <- subjectCPMcutoff

  annotate <- pf[grep("annotate", pf)]
  annotate <- sub("true", annotation, annotate)
  pf[grep("annotate", pf)] <- annotate

  zz <- file("Parameters.txt", "w")
  writeLines(pf, zz)
  close(zz)

  zz <- file(paste(scrat_tmp.folder,"/samples/Parameters.txt", sep=""), "w")
  writeLines(pf, zz)
  close(zz)

  if(group=="sudo"){
    params <- paste("--cidfile ", samples.folder,"/dockerID -v ", paste(scrat_tmp.folder,"/samples", sep=""),":/samples -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starChipCircle.sh", sep="")
    resultRun <- runDocker(group="sudo",container="docker.io/repbioinfo/star251.2017.01", params=params)
  }else{
    params <- paste("--cidfile ", samples.folder,"/dockerID -v ", paste(scrat_tmp.folder,"/samples", sep=""),":/samples -v ", genome.folder,":/genome -d docker.io/repbioinfo/star251.2017.01 sh /bin/starChipCircle.sh", sep="")
    resultRun <- runDocker(group="docker",container="docker.io/repbioinfo/star251.2017.01", params=params)
  }

  if(resultRun=="false"){
    cat("\nstarchipCircle runs are finished\n")
    system(paste("cp -R", scrat_tmp.folder,"/samples/rawdata ", samples.folder, sep=""))
    system(paste("cp ", scrat_tmp.folder,"/samples/* ", samples.folder, sep=""))
  }

  #running time 2
  ptm <- proc.time() - ptm
  con <- file(paste(samples.folder,"run.info", sep="/"), "r")
  tmp.run <- readLines(con)
  close(con)
  tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
  tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
  writeLines(tmp.run,paste(samples.folder,"run.info", sep="/"))
  #running time 2
  #removing temporary folder
  #saving log and removing docker container
  container.id <- readLines(paste(samples.folder,"/dockerID", sep=""), warn = FALSE)
  #    system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
  system(paste("docker logs ", container.id, " >& ","starchipCircle_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))


  cat("\n\nRemoving the starChipIndex temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system(paste("rm  -f ",samples.folder,"/dockerID", sep=""))
  setwd(home)

}
