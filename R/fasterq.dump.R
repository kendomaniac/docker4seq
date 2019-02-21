#' @title A function to handle fasterq-dumper SRA to download SRA fastq files
#' @description This function executes a ubuntu docker that produces as output FASTQCstdin_fastqc.html and stdin_fastqc.zip files
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param sra.name, a character string indicating the name of the SRA object to be download
#' @param data.folder, a character string indicating the working folder where output folder will be written
#' @param scratch.folder, a character string indicating the temporary folder for data preprocessing
#' @param threads, a integer indicating the number of threads to be used from fasterq-dumper
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino
#'
#' @examples
#' \dontrun{
#'     #running sraDownload
#'     sraDownload(group="docker", sra.name="SRR7762358", data.folder=getwd(), scratch.folder="/data/scratch", threads=8)
#'     system("mv ./SRR7762358/SRR7762358.fastq.gz ./SRR7762358/SRR7762358_S1_L001_R1_001.fastq.gz")
#' }
#'
#' @export
sraDownload <- function(group=c("sudo","docker"), sra.name, data.folder, scratch.folder, threads=8){


  #storing the position of the home folder
  home <- getwd()



  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }

  setwd(data.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")


  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }


  #executing the docker job
  params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder, ":/workingdir -v ", scratch.folder, ":/tmpfolder  -d docker.io/repbioinfo/sra.2018.01 bash /bin/sra.download.sh ", sra.name, " ", threads, sep="")
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nsra fasterq dump is finished\n")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("sra fasterq dump user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sra fasterq dump system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sra fasterq dump elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("sra fasterq dump user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sra fasterq dump system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("sra fasterq dump elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/sraDownload_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
