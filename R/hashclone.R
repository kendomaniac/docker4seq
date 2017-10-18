#' @title HashClone running A function to handle a hashclone docker containier
#' @description This function executes  HashClone algorithm developed to identify the set of clonality markers during the patient follow-up in order to quantify the minimal residual disease. 
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @author Beccuti Marco, Greta Romano, Francesca Cordero, Raffaele Calogero, beccuti@di.unito.it, Computer Science Department Univ. of Turin.
#' 
#' @examples
#' \dontrun{
#'     #running skeleton
#'     skeleton(group="docker", scratch.folder="/Users/raffaelecalogero/Desktop/scratch", 
#'     data.folder=getwd())
#' }
#'
#' @export
hashclone <- function(group=c("sudo","docker"), scratch.folder, data.folder=getwd(), kmer, hash,  coll, threashold=0.1, spike="null", input.files){
  
  #docker image
  dockerImage="docker.io/beccuti/hashclone"
  
  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  
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
  
  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
  cat("\nCreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  
  #executing the docker job
  
  common_params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d ",dockerImage,sep="")
  
  parms <- paste(common_params," /usr/HashClone/HashCheckerFreq ",sep="")
  for i in 1:length(input.files){
    parms <- paste(common_params,data.folder,"/input-",input.files[i]," fastq 1 ",common_params,data.folder,"/input-",input.files[i],  "fastq 1 ",common_params,data.folder,"/output-",input.files[i]," ",kmer," 1 ",hash," ",coll, sep=""
    resultRun <- runDocker(group=group,container=dockerImage, params=params)
   }

  #waiting for the end of the container work
  if(!resultRun){
    system(paste("cp ", scrat_tmp.folder, "/* ", data.folder, sep=""))
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
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),".log", sep=""))
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
