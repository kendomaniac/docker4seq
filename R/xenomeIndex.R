#' @title Generating xenome genome indexes 
#' @description This function executes the docker container xenome.2017.01 where xenome is installed. 
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param xenome.folder, a character string indicating the folder where the indexed reference genomes for xenome will be located
#' @param hg.urlgenome, a character string indicating the URL from uscs download web page for the unmasked human genome sequence of interest
#' @param mm.urlgenome, a character string indicating the URL from uscs download web page for the unmasked mouse genome sequence of interest
#' @param threads, an integer indicating how many threads are used by xenome
#' @author Raffaele Calogero
#'
#' @return The indexed xenome genomes references
#' @examples
#'\dontrun{
#'     #running xenome index
#'     xenomeIndex(group="docker",xenome.folder="/data/scratch/test", hg.urlgenome=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz",
#'     mm.urlgenome="http://hgdownload.cse.ucsc.edu/goldenPath/mm10/bigZips/chromFa.tar.gz")
#'
#' }
#' @export
xenomeIndex <- function(group=c("sudo","docker"),xenome.folder=getwd(), hg.urlgenome=NULL, mm.urlgenome=NULL, threads=8){

  home <- getwd()
  setwd(xenome.folder)
  
  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")
  
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }

    #########check scratch folder exist###########
  if (!file.exists(xenome.folder)){
    cat(paste("\nIt seems that the ",xenome.folder, "folder does not exist, I create it\n"))
    dir.create(xenome.folder)
  }
  #############################################

	cat("\nsetting as working dir the genome folder and running bwa docker container\n")
  
  params <- paste("--cidfile ",xenome.folder,"/dockerID -v ",xenome.folder,":/data/scratch"," -d docker.io/repbioinfo/xenome.2017.01 sh /bin/xenome.index.sh ", hg.urlgenome, " ", mm.urlgenome, " ",threads, sep="")
	
  resultRun <- runDocker(group="docker", params=params)

  if(resultRun==0){
    cat("\nXenome genome indexes were created\n")
  }
  
	#running time 2
	ptm <- proc.time() - ptm
	con <- file(paste(xenome.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)

	tmp.run <- NULL
	tmp.run[length(tmp.run)+1] <- paste("xenome user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("xenome system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("xenome elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run, paste(xenome.folder,"run.info", sep="/"))
  #running time 2

	
	#saving log and removing docker container
	container.id <- readLines(paste(xenome.folder,"/dockerID", sep=""), warn = FALSE)
#	system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker logs ", container.id, " >& ","xenomeIndex_",substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))
	system(paste("rm  -f ",xenome.folder,"/dockerID", sep=""))
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",xenome.folder, sep=""))
	setwd(home)
}

