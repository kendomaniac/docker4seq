#' @title Running Star to detect chimeric transcripts on paired-end sequences
#' @description This function executes STAR to detect chimeric transcripts
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param fastq.folder, a character string indicating where gzip fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param threads, a number indicating the number of cores to be used from the application
#' @param chimSegmentMin, is a positive integer indicating the minimal length of the overlap of a read to the chimeric element
#' @param chimJunctionOverhangMin, is a positive integer indicating the minimum overhang for a chimeric junction
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @return the set of chimeric transcripts identified by STAR chimeric
#' @examples
#'\dontrun{
#'     #downloading fastq files
#'     system("wget http://130.192.119.59/public/test_R1.fastq.gz")
#'     system("wget http://130.192.119.59/public/test_R2.fastq.gz")
#'     #running star2step nostrand pe
#'     starChimeric(group="docker",fastq.folder=getwd(), scratch.folder="/data/scratch",
#'     genome.folder="/data/scratch/hg38star", threads=8, chimSegmentMin=20, chimJunctionOverhangMin=15)
#'
#' }
#' @export


starChimeric <- function(group=c("sudo","docker"), fastq.folder=getwd(), scratch.folder,
    genome.folder, threads=1, chimSegmentMin=20, chimJunctionOverhangMin=15) {

    scratch.folder <- normalizePath(scratch.folder)
    fastq.folder <- normalizePath(fastq.folder)
    genome.folder <- normalizePath(genome.folder)

    #running time 1
    ptm <- proc.time()
    #setting the data.folder as working folder
    if (!file.exists(fastq.folder)) {
      cat(paste("\nIt seems that the", fastq.folder, "folder does not exist\n"))
      return(2)
    }

    data.folder <- fastq.folder

    #storing the position of the home folder
    home <- getwd()
    setwd(fastq.folder)
    #initialize status
    system("echo 0 > ExitStatusFile 2>&1")

    #check if scratch folder exist
    if (!file.exists(scratch.folder)) {
      cat(paste("\nIt seems that the", scratch.folder, "folder does not exist\n"))
      system("echo 3 > ExitStatusFile 2>&1")
      setwd(data.folder)
      return(3)
    }
    #check if genome folder exist
    if (!file.exists(genome.folder)) {
      cat(paste("\nIt seems that the", genome.folder, "folder does not exist\n"))
      system("echo 3 > ExitStatusFile 2>&1")
      setwd(data.folder)
      return(3)
    }

    samples <- dir(path=fastq.folder)
    samples <- samples[grep(".fastq.gz$", samples)]
    trimmed.samples <- samples[grep("trimmed", samples)]

    if (length(samples) == 0) {
        cat(paste("It seems that in", fastq.folder, "there are not fastq.gz files"))
        system("echo 1 > ExitStatusFile 2>&1")
        setwd(home)
        return(1)
    } else if (length(trimmed.samples) > 0) {
        samples <- trimmed.samples
    } else if (length(samples) > 2) {
        cat(paste("It seems that in", fastq.folder, "there are more than two fastq.gz files"))
        system("echo 2 > ExitStatusFile 2>&1")
        setwd(home)
        return(2)
    }

    #executing the docker job
    params <- paste("--cidfile", paste0(fastq.folder, "/dockerID"),
        "-v", paste0(fastq.folder, ":/fastq.folder"),
        "-v", paste0(genome.folder, ":/genome"),
        "-v", paste0(scratch.folder, ":/scratch"),
        "-d docker.io/repbioinfo/star251.2019.02 /bin/bash /bin/start_chimeric.sh",
        chimSegmentMin, chimJunctionOverhangMin, threads, samples[1], samples[2])

    resultRun <- runDocker(group=group, params=params)

    if(resultRun == 0) {
      cat("\nSTAR to detect chimeric transcripts is finished\n")
    }

    #running time 2
    ptm <- proc.time() - ptm
    dir <- dir(data.folder)
    dir <- dir[grep("run.info",dir)]
    if(length(dir)>0) {
      con <- file("run.info", "r")
      tmp.run <- readLines(con)
      close(con)
      tmp.run[length(tmp.run)+1] <- paste("STAR chimeric user run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("STAR chimeric system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("STAR chimeric elapsed run time mins ",ptm[3]/60, sep="")
      writeLines(tmp.run,"run.info")
    }
    else {
      tmp.run <- NULL
      tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("STAR chimeric system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("STAR chimeric elapsed run time mins ",ptm[3]/60, sep="")

      writeLines(tmp.run,"run.info")
    }

    #saving log and removing docker container
    container.id <- readLines(paste0(data.folder,"/dockerID"), warn = FALSE)
    system(paste("docker logs", substr(container.id,1,12), "&>", paste0("starChimeric_", substr(container.id,1,12),".log")))
    system(paste("docker rm", container.id))
    # removing temporary files
    cat("\n\nRemoving the temporary file ....\n")
    system("rm -fR out.info")
    system("rm -fR dockerID")
    system(paste("cp", paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/"), data.folder))
    setwd(home)
}
