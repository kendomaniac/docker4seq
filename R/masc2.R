#' @title A function to handle a MACS2 containier
#' @description This function executes a MACS2 docker that produces as output peaks call
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param control.bam, aa character string indicating the path to the control bam file. IMPORTANT control.bam and chipseq.bam are in the same folder
#' @param chipseq.bam, a character string indicating the path to the chipseq bam file. IMPORTANT control.bam and chipseq.bam are in the same folder
#' @param experiment.name, a character string indicating the prefix for MCS2 output


#' @author raffaele.calogero [at] unito [dot] it, University of Torino
#' 
#' @examples
#' \dontrun{
#'     #running MACS
#'     macs2(group="docker", control.bam=paste(getwd(),"igg_dedup_reads.bam", sep="/"), 
#'            chipseq.bam=paste(getwd(),"prdm51_dedup_reads.bam", sep="/"), experiment.name="prdm51_igg")
#' }
#'
#' @export
macs2 <- function(group=c("sudo","docker"), control.bam, chipseq.bam, experiment.name){

  #running time 1
  ptm <- proc.time()
  
  ctrl.folder=dirname(control.bam)
  chipseq.folder=dirname(chipseq.bam)
  if(ctrl.folder != ctrl.folder){
    cat("\nctrl.bam and chipseq.bam MUST be in the same folder\n")
    return(1)
    
  }else{
    data.folder=ctrl.folder
    controlC=basename(control.bam)
    chipseqC=basename(chipseq.bam)
  }
  
  #storing the position of the home folder  
  home <- getwd()
  setwd(data.folder)
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  

  #setting the data.folder as working folder
  if (!file.exists(data.folder)){
    cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
    return(2)
  }
  

  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1") 
    setwd(home)
    return(10)
  }
  
  

#executing the docker job
  params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data -d fooliu/macs2 callpeak -t /data/", chipseqC," -c /data/", controlC, " -n ", experiment.name, " --outdir /data", sep="")
  resultRun <- runDocker(group=group, params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nMACS2 analysis is finished\n")
    model <- dir()
    model <- model[grep(".r$", model)]
    system(paste("Rscript", model, sep=" "))
  }
  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("MACS2 user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MACS2 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MACS2 elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("MACS2 run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MACS2 system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("MACS2 elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),"_MACS2.log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
