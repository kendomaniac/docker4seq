#' @title A function executing kraken
#' @description This function executes a ubuntu docker that embed kraken2
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param fastq.folder, a character string indicating the folder where input fastq(s) are located and where output will be written
#' @param fastq.prefix, a character string indicating the PREFIX of the fastq file(s): e.g. prova_R1.fastq.gz fastq.prefix="prova"
#' @param genome.folder, a character string indicating the folder where the indexed kraken db is located
#' @param threads, a number indicating the number of cores to be used from the application

#' @author Raffaele A Calogero, raffaele.calogero [at] unito [dot] it, University of Torino
#' 
#' @examples
#' \dontrun{
#'     #running skeleton
#'     kraken(group="docker", scratch.folder="/data/scratch", 
#'     fastq.folder=getwd(), genome.folder="/data/genomes/minikraken",
#'     fastq.prefix="unmapped", threads=8)
#' }
#'
#' @export
kraken <- function(group="docker", scratch.folder, fastq.folder, genome.folder, fastq.prefix, threads){

  #running time 1
  ptm <- proc.time()
  #setting the fastq.folder as working folder
  if (!file.exists(fastq.folder)){
    cat(paste("\nIt seems that the ",fastq.folder, " folder does not exist\n"))
    return(2)
  }
  
  #storing the position of the home folder  
  home <- getwd()
  setwd(fastq.folder)
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
    setwd(fastq.folder)
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(fastq.folder,"/tempFolderID", sep=""))
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  
  ### copying fastq files 
  dir <- dir(fastq.folder)
  dir <- dir[grep(".fastq.gz", dir)]
  dir <- dir[grep(fastq.prefix, dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", fastq.folder, "there are not fastq.gz files"))
    system("echo 1 >& ExitStatusFile")
    setwd(home)
    return(1)
  }else if(length(dir)>2){
    cat(paste("It seems that in ", fastq.folder, "there are more than two fastq.gz files"))
    system("echo 2 >& ExitStatusFile")
    setwd(home)
    return(2)
  }else{
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
    }
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
  }
  #Trimmed fastq  linking fpr docker
  docker_fastq.folder=scrat_tmp.folder
  
  if(length(dir)==2){
    params <- paste("--cidfile ", fastq.folder,"/dockerID -v ", genome.folder, ":/reference -v ",scrat_tmp.folder,":/scratch -v ", fastq.folder, ":/data -d docker.io/repbioinfo/kraken.2019.01 /bin/kraken_run_pe.sh ", dir[1], " ", dir[2], " ", threads, sep="")
  }else{
    params <- paste("--cidfile ", fastq.folder,"/dockerID -v ", genome.folder, ":/reference -v ",scrat_tmp.folder,":/scratch -v ", fastq.folder, ":/data -d docker.io/repbioinfo/kraken.2019.01 /bin/kraken_run_se.sh ", dir[1], " ", threads, sep="")
  }
  #executing the docker job
  resultRun <- runDocker(group=group, params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    cat("Kraken analysis is finshed")
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(fastq.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("kraken user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("kraken system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("kraken elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("kraken run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("kraken system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("kraken elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",fastq.folder,"/", substr(container.id,1,12),"_kraken.log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
  setwd(home)
}
