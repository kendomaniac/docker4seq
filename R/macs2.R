#' @title A function to handle a MACS2 containier
#' @description This function executes a MACS2 docker that produces as output peaks call
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param control.bam, a character string indicating the path to the control bam file. IMPORTANT control.bam and chipseq.bam are in the same folder
#' @param chipseq.bam, a character string indicating the path to the chipseq bam file. IMPORTANT control.bam and chipseq.bam are in the same folder
#' @param experiment.name, a character string indicating the prefix for MCS2 output
#' @param histone.marks, boolean if TRUE activate the broad option to call histone marks
#' @param broad.cutoff, if histone.mark is TRUE broad.cutoff can be set, default 0.1
#' @param qvalue, The qvalue (minimum FDR) cutoff to call significant regions. Default is 0.05. For broad marks, you can try 0.05 as cutoff.
#' @param organism, required to select the correct genome size avaialble options hs, mm
#' @return NAME_peaks.xls, which is a tabular file which contains information about called peaks. You can open it in excel and sort/filter using excel functions. Information include: chromosome name, start position of peak, end position of peak, length of peak region, absolute peak summit position, pileup height at peak summit, -log10(pvalue) for the peak summit (e.g. pvalue =1e-10, then this value should be 10), fold enrichment for this peak summit against random Poisson distribution with local lambda, -log10(qvalue) at peak summit. NAME_peaks.narrowPeak is BED6+4 format file which contains the peak locations together with peak summit, pvalue and qvalue. You can load it to UCSC genome browser. Definition of some specific columns are: 5th: integer score for display calculated as int(-10*log10qvalue). Please note that currently this value might be out of the [0-1000] range defined in UCSC Encode narrowPeak format, 7th: fold-change, 8th: -log10pvalue, 9th: -log10qvalue, 10th: relative summit position to peak start. NAME_peaks.broadPeak is in BED6+3 format which is similar to narrowPeak file.   


#' @author raffaele.calogero [at] unito [dot] it, University of Torino
#' 
#' @examples
#' \dontrun{
#'     #running MACS for conventional peaks
#'     macs2(group="docker", control.bam=paste(getwd(),"igg_dedup_reads.bam", sep="/"), 
#'             chipseq.bam=paste(getwd(),"prdm51_dedup_reads.bam", sep="/"), 
#'             experiment.name="prdm51_igg", histone.marks=FALSE, qvalue=0.05, 
#'             organism="mm")
#'             
#'     #running MACS for histone marks
#'     macs2(group="docker", control.bam=paste(getwd(),"igg_dedup_reads.bam", sep="/"), 
#'             chipseq.bam=paste(getwd(),"prdm51_dedup_reads.bam", sep="/"), 
#'             experiment.name="prdm51_igg", histone.marks=TRUE, broad.cutoff=0.1, 
#'             organism="mm")     
#' }
#'
#' @export
macs2 <- function(group=c("sudo","docker"), control.bam, chipseq.bam, experiment.name, histone.marks=FALSE, broad.cutoff=0.1, qvalue=0.05, organism=c("hs", "mm")){

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
  
  
  if(organism=="hs"){
    g.size=2.7e9
  }else if(organism=="mm"){
    g.size=1.87e9
  }else{
    cat("\nNot imple mented, yet!\n")
    return(3)
  }
  
  
  
#executing the docker job
  if(!histone.marks){
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data -d fooliu/macs2 callpeak --qvalue ", qvalue, " --gsize ", g.size, " -t /data/", chipseqC," -c /data/", controlC, " -n ", experiment.name, " --outdir /data", sep="")
  }else{
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data -d fooliu/macs2 callpeak --broad --broad-cutoff ", broad.cutoff, " --gsize ", g.size, " -t /data/", chipseqC," -c /data/", controlC, " -n ", experiment.name, " --outdir /data", sep="")
  }
  
  
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
