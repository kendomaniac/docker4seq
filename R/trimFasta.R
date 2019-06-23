#' @title A function to execute seqtk trimming
#' @description This function executes a ubuntu docker that remove reads shorter of a specific threshold
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param min.length, min nucleotide lenght of each fasta seq
#' 
#' @author Raffaele A Calogero, raffaele.calogero [at] unito [dot] it, University of Torino. Italy
#'
#' @return Returns a fasta file colled trimmed.fasta.gz
#' @examples
#' \dontrun{
#'     #running fastq2fasta
#'     trimfasta(group="docker", scratch.folder="/data/scratch", data.folder=getwd(), min.length=300)
#' }
#'
#' @export
trimFasta <- function(group=c("sudo","docker"), scratch.folder, data.folder, min.length=300){


#running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }
  
  #storing the position of the home folder  
  home <- getwd()
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
  

  
  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(data.folder)
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  
  ### copying fastq files 
  dir <- dir(data.folder)
  dir <- dir[grep(".fasta.gz$", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", data.folder, "there are not fasta files"))
    system("echo 1 >& ExitStatusFile")
    setwd(home)
    return(1)
  }else if(length(dir)>1){
    cat(paste("It seems that in ", data.folder, "there is more than 1 fasta files"))
    system("echo 2 >& ExitStatusFile")
    setwd(home)
    return(2)
  }else{
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",data.folder,"/",dir, " ",scratch.folder,"/",tmp.folder,"/",dir, sep=""))
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }
  ###
  
  #executing the docker job
  params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/repbioinfo/cdhit.2019.01:cdhit-V4.8.1 bash /bin/trim.sh ", dir, " ", min.length, sep="")
  resultRun <- runDocker(group=group, params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nTrimming is finished \n")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("trimFasta user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("trimFasta system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("trimFasta elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("trimFasta run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("trimFasta system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("trimFasta elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),"_trimFasta.log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
