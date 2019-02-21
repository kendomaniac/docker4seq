#' @title Detection and annotation of TF binding sites and histone marks, based on the workflow used in Galli et al Mol Cell. 2015 Oct 15;60(2):328-37
#' @description This function executes the docker container chipseq.8 and requires as input two bam files, one for the chipseq of interest and the other for control, e.g. mock, which can be generated with bwa function.
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param bam.folder, a character string indicating where bam files are located
#' @param sample.bam, a character string indicating the chipseq file under analysis
#' @param ctrl.bam, a character string indicating the control file, e.g. unspecific IgG, input DNA, etc.
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome, a character string indicating the genome used as reference for data generation. Available options: hg19, hg38, mm9, mm10
#' @param read.size, an integer indicating the length of the sequenced reads
#' @param tool, a character string indicating the peaks calling algorith. Available options: macs and sicer. Macs, v 1.14, is used to call TF peaks, as instead sicer, v 1.1, is used to call histone mark peaks
#' @param macs.min.mfold, an integer indicating the minimum enrichment ratio against background
#' @param macs.max.mfold, an integer indicating the maximum enrichment ratio against background
#' @param macs.pval, a character string, indicationg the pvalue cutoff to be used to filter peaks with low statistical significance.The number must be provided in scientific notation as the default value shows
#' @param sicer.wsize, an integer indicating the windows size to be used by sicer
#' @param sicer.gsize, an integer indicating the gap size to be used by sicer. Suggested values: H3K4Me3=200; H3K27Me3=600
#' @param sicer.fdr, an integer indicating the pvalue cutoff to be used to filter peaks with low statistical significance
#' @param tss.distance, an integer indicating the distance of TSS with respect to gene start
#' @param max.upstream.distance, an integer indicating the maximum distance to associate a gene ID to a peak
#' @param remove.duplicates, a character string indicating if duplicated reads have to be removed. Available options: Y, to remove douplicates, N to keep duplicates
#'
#' @return three files: dedup_reads.bam, which is sorted and duplicates marked bam file, dedup_reads.bai, which is the index of the dedup_reads.bam, and dedup_reads.stats, which provides mapping statistics
#' @examples
#'\dontrun{
	#'     system("wget http://130.192.119.59/public/SRR1172111.bam")#TEAD
	#'     system("wget http://130.192.119.59/public/SRR1172110.bam")#igg
	#'     system("wget http://130.192.119.59/public/SRR1592211.bam")#H3K27ac
	#'     #running chipseq for macs
	#'     chipseq(group="sudo",bam.folder=getwd(), sample.bam="SRR1172111.bam", ctrl.bam="SRR1172110.bam",
	#'     scratch.folder="/data/scratch", genome="hg19", read.size=50,
	#'     tool="macs", macs.min.mfold=10, macs.max.mfold=30, macs.pval="1e-5",
	#'     sicer.wsize=200, sicer.gsize=200, sicer.fdr=0.10, tss.distance=0, max.upstream.distance=10000,
	#'     remove.duplicates="N")
	#'
	#'     #running chipseq for sicer H3K4Me3
	#'     chipseq(group="sudo",bam.folder=getwd(), sample.bam="SRR1592211.bam", ctrl.bam="SRR1172110.bam",
	#'     scratch.folder="/data/scratch", genome="hg19", read.size=50,
	#'     tool="sicer", sicer.wsize=200, sicer.gsize=200, sicer.fdr=0.10,
	#'     tss.distance=0, max.upstream.distance=10000,remove.duplicates="N")
#' }
#' @export
chipseq <- function(group=c("sudo","docker"), bam.folder=getwd(), sample.bam, ctrl.bam, scratch.folder="/data/scratch", genome=c("hg19","hg38","mm9","mm10"), read.size, tool=c("macs","sicer"), macs.min.mfold=10, macs.max.mfold=30, macs.pval="1e-5", sicer.wsize=200, sicer.gsize=c(200,600), sicer.fdr=0.10, tss.distance=0, max.upstream.distance=10000,  remove.duplicates=c("Y","N")){
  #running time 1
  ptm <- proc.time()
  #running time 1


  #remembering actual folder
  home <- getwd()

  setwd(bam.folder)

  #initialize status
  system("echo 0 > ExitStatusFile 2>&1")

  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 > ExitStatusFile 2>&1")
    setwd(home)
    return(10)
  }

  #########check scratch folder exist###########
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, "folder does not exist\n"))
    system("echo 3 > ExitStatusFile 2>&1")
    setwd(home)
    return(3)
  }
  #############################################

  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
	cat("\ncreating a folder in scratch folder\n")
	scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
	writeLines(scrat_tmp.folder,paste(bam.folder,"/tempFolderID", sep=""))
  dir.create(file.path(scratch.folder, tmp.folder))

  docker_chipseq.folder=file.path("/data/scratch", tmp.folder)

  dir <- dir(path=bam.folder)
  dir.info <- dir[which(dir=="run.info")]
  if(length(dir.info)>0){
    system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
    system(paste("cp ",bam.folder,"/run.info ", scratch.folder,"/",tmp.folder,"/run.info", sep=""))

  }

	dir <- dir[grep(".bam$", dir)]
	dir <- dir[which(dir%in%c(sample.bam, ctrl.bam))]
	cat("\ncopying \n")
	if(length(dir)==0){
		cat(paste("It seems that in ", bam.folder, "there are not bam files"))
	  	system("echo 1 > ExitStatusFile 2>&1")
 		setwd(home)
		return(1)
	}else if(length(dir)>2){
		cat(paste("It seems that in ", bam.folder, "there are more than two bam files"))
	  	system("echo 2 > ExitStatusFile 2>&1")
 		setwd(home)
		return(2)
	}else{
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
		for(i in dir){
		      system(paste("cp ",bam.folder,"/",i, " ",scratch.folder,"/",tmp.folder,"/",i, sep=""))
	    }
		system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
	}
	cat("\nsetting as working dir the scratch folder and running chipseq docker container\n")

params <- paste("--cidfile ", bam.folder,"/dockerID -v ",scratch.folder,":/data/scratch"," -d docker.io/repbioinfo/chipseq.2017.01 /usr/local/bin/Rscript /wrapper.R ",sample.bam, " ",bam.folder," ", ctrl.bam," 000000 ",docker_chipseq.folder," ",genome," ",read.size," ",tool," ",macs.min.mfold," ",macs.max.mfold," ", macs.pval," ",sicer.wsize," ", sicer.gsize," ",sicer.fdr," ",tss.distance," ",max.upstream.distance," ",remove.duplicates, sep="")

resultRun=runDocker(group=group, params=params)


	if(resultRun==0){
	  cat("\nChipseq is finished\n")
	}



#	system(paste("chmod 777 -R", file.path(scratch.folder, tmp.folder)))
#	con <- file(paste(scrat_tmp.folder,"out.info", sep="/"), "r")
#	tmp <- readLines(con)
#	close(con)
#	for(i in tmp){
#		i <- sub("cp ",paste("cp ",scrat_tmp.folder,"/",sep=""),i)
#		system(i)
#	}

  system(paste("cp ",scrat_tmp.folder,"/*.xls ",bam.folder,sep=""))
  system(paste("cp ",scrat_tmp.folder,"/*.info ",bam.folder,sep=""))
  system(paste("cp ",scrat_tmp.folder,"/*.counts ",bam.folder,sep=""))
  system(paste("cp ",scrat_tmp.folder,"/*.bed ",bam.folder,sep=""))
  system(paste("cp ",scrat_tmp.folder,"/*.bw ",bam.folder,sep=""))
  system(paste("cp ",scrat_tmp.folder,"/*.pdf ",bam.folder,sep=""))
  system("mv mypeaks.annotated.xls_entrez.xls mypeaks.xls")
	#running time 2
	ptm <- proc.time() - ptm
	con <- file(paste(bam.folder,"run.info", sep="/"), "r")
	tmp.run <- readLines(con)
	close(con)
	tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
	tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
	writeLines(tmp.run,paste(bam.folder,"run.info", sep="/"))
	#running time 2

	#saving log and removing docker container
	container.id <- readLines(paste(bam.folder,"/dockerID", sep=""), warn = FALSE)
	system(paste("docker logs ", container.id, " &> ", substr(container.id,1,12),".log", sep=""))
	system(paste("docker rm ", container.id, sep=""))

	#removing temporary folder
	cat("\n\nRemoving the rsemStar temporary file ....\n")
	system(paste("rm -R ",scrat_tmp.folder))
  system(paste("rm  ",bam.folder,"/dockerID", sep=""))
  system(paste("rm  ",bam.folder,"/tempFolderID", sep=""))
	#removing temporary folder
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",bam.folder, sep=""))

  setwd(home)
}
