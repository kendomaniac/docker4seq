#' @title HashClone running A function to handle a hashclone docker container NOT READY for STABLE RELEASE, YET
#' @description This function executes  HashClone algorithm developed to identify the set of clonality markers during the patient follow-up in order to quantify the minimal residual disease.
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param kmer, an integer that define the size of the substrings (k-mer) encoded in the hash table (this must be a value between 1 and 32)
#' @param hash, a prime number indicating the size of the hash table. Increasing this value reduces the execution time but increases the memory utilization. Ideally, this value should be close to the number of different k-mers stored in the hash table;
#' @param coll, an integer that define the maximum number of different k-mers that the tool might need to store in the hash table.
#' @param threshold, (tau) this value is the threshold used to select significant k-mers. We suggest to set tau equal to 1
#' @param type, IGH (immunoglobulin heavy chain) or  IGK (immunoglobulin kappa locus)
#'@param spike, a character string indicating the path of the spike in file (if you don't want the spike in research, please set this parameter as 'null')
#' @param input.files, a character string indicating the path of the input files
#' @author Beccuti Marco, Greta Romano, Francesca Cordero, Raffaele Calogero, beccuti[at]di[dot]unito[dot]it, Computer Science Department Univ. of Turin.
#'
#' @examples
#' \dontrun{
#' library(docker4seq)
#' downloadContainers(group="docker","docker.io/qbioturin/hashclone")
#' hashclone(group="docker",scratch.folder="/home/scratch_folder", data.folder="/home/output_folder", kmer=26, hash=10999997, coll=10999997, threshold=1, type= IGH, spike="/home/spike_in.fa", input.files=c('/home/input_file1.fastq', '/home/input_file2.fastq'))
#' }
#'
#'
#' @export
hashclone <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), kmer, hash,  coll, threshold=0.1, type ,spike="null", input.files){

  #docker image
  dockerImage="docker.io/repbioinfo/hashclone.2019.01"

  scratch.folder <- normalizePath(scratch.folder)
  data.folder <- normalizePath(data.folder)


  #storing the position of the home folder
  home <- getwd()


  #running time 1
  ptm <- proc.time()

  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the", data.folder, "folder does not exist\n"))
    system("echo 2 > ExitStatusFile 2>&1")
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



  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\nCreating a folder in scratch folder\n")
  scrat_tmp.folder=file.path(scrat_tmp.folder)
  dir.create(scrat_tmp.folder)
  #copying input files
  cat("\nCopying input files in scratch folder\n")
  for (i in 1:length(input.files)){
    if (system(paste("cp ",input.files[i],scrat_tmp.folder),intern = FALSE) != 0)
      {
      cat("Error input file ",input.files[i],scrat_tmp.folder," does not exist\n")
      system("echo 4 > ExitStatusFile 2>&1")
      setwd(home)
      return(4)
      }

  }
  if (spike!="null")
      if (system(paste("cp ",spike,scrat_tmp.folder)))
      {
      cat("Error input file ",input.files[i],scrat_tmp.folder," does not exist\n")
      system("echo 4 > ExitStatusFile 2>&1")
      setwd(home)
      return(4)
      }
  #executing the docker job

  params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d ",dockerImage, " /bin/hashclone.sh ",kmer," ",hash," ",coll," ",threshold," ",type ," "," /scratch  null ", sep="")
  if  (spike!="null")
    params <- paste(params,paste("/scratch/",basename(spike),sep=""), sep=" ")
  else
    params <- paste(params,"null", sep=" ")

  for (i in 1:length(input.files)){
    params <- paste(params,paste("/scratch/",basename(input.files[i]),sep=""), sep=" ")
  }

  #Run docker
  resultRun <- runDocker(group=group, params=params)
  #waiting for the end of the container work
  if(resultRun==0){
    system(paste("cp ", scrat_tmp.folder, "/*.csv ", data.folder, sep=""))
    system(paste("cp ", scrat_tmp.folder, "/*.csv.gui ", data.folder, sep=""))
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/hashclone_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm", container.id))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -fR ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)

}
