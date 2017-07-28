#' @title Generating xenome genome indexes 
#' @description This function executes the docker container xenome.2017.01 where xenome is installed. 
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param xenome.folder, a character string indicating the folder where the indexed reference genomes for xenome will be located
#' @param hg.urlgenome, a character string indicating the URL from uscs download web page for the unmasked human genome sequence of interest
#' @param mm.urlgenome, a character string indicating the URL from uscs download web page for the unmasked mouse genome sequence of interest
#' @param threads, an integer indicating how many threads are used by xenome
#'
#' @return The indexed xenome genomes reference sequences
#' @examples
#'\dontrun{
#'     #running bwa index
#'     xenomeIndex(group="docker",xenome.folder="/data/scratch/test", hg.urlgenome=
#'     "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz",
#'     mm.urlgenome="http://hgdownload.cse.ucsc.edu/goldenPath/mm10/bigZips/chromFa.tar.gz")
#'
#' }
#' @export
xenomeIndex <- function(group=c("sudo","docker"),xenome.folder=getwd(), hg.urlgenome=NULL, mm.urlgenome=NULL, threads=8){
  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }

    #########check scratch folder exist###########
  if (!file.exists(xenome.folder)){
    cat(paste("\nIt seems that the ",xenome.folder, "folder does not exist, I create it\n"))
    dir.create(xenome.folder)
  }
  #############################################

	cat("\nsetting as working dir the genome folder and running bwa docker container\n")
  
	if(group=="sudo"){
		params <- paste("--cidfile ",xenome.folder,"/dockerID -v ",xenome.folder,":/data/scratch"," -d docker.io/repbioinfo/xenome.2017.01 sh /bin/xenome.index.sh ", hg.urlgenome, " ", mm.urlgenome, " ",threads, sep="")
		runDocker(group="sudo",container="docker.io/repbioinfo/xenome.2017.01", params=params)
	}else{
	  params <- paste("--cidfile ",xenome.folder,"/dockerID -v ",xenome.folder,":/data/scratch"," -d docker.io/repbioinfo/xenome.2017.01 sh /bin/xenome.index.sh ", hg.urlgenome, " ", mm.urlgenome, " ",threads, sep="")
	  runDocker(group="docker",container="docker.io/repbioinfo/xenome.2017.01", params=params)
	}
	out <- "xxxx"
	#waiting for the end of the container work
	while(out != "out.info"){
	  Sys.sleep(10)
	  cat(".")
	  out.tmp <- dir(xenome.folder)
	  out.tmp <- out.tmp[grep("out.info",out.tmp)]
	  if(length(out.tmp)>0){
	    out <- "out.info"
	  }
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
	system(paste("rm ",xenome.folder,"/out.info",sep=""))
	
	#saving log and removing docker container
	container.id <- readLines(paste(xenome.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " >& ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))
	
	system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",xenome.folder, sep=""))
}

