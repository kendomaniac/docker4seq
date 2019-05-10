#' @title hfc, heatmap for clustering
#' @description This function generate heatmap and other plot based on clustering and on a specific gene list
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param file, a character string indicating the path of the file, with counts.table name and extension included
#' @param status, 0 if is raw count, 1 otherwise
#' @param lower.range, the lower range of signal in the heatmap.
#' @param upper.range, the upper range of signal in the heatmap.
#' @author Luca Alessandri , alessandri [dot] luca1991 [at] gmail [dot] com, University of Torino
#'
#' @return A heatmap.
#' @examples
#'\dontrun{
#' system("wget http://130.192.119.59/public/heatmap_test.zip")
#' system("unzip heatmap_test.zip")
#' setwd("heatmap_test")
#' heatmapBase(group="docker",scratch.folder="/data/scratch",file=paste(getwd(),"DEfiltered__log2TPM.txt", sep="/"), status=1, lower.range=-1, upper.range=1)

#'}
#' @export
heatmapBase <- function(group=c("sudo","docker"), scratch.folder, file, status=0, lower.range=-1, upper.range=1){

  b1=lower.range
  b2=upper.range
  if(b1<0){b1=paste("/",b1,sep="")}
    if(b2<0){b2=paste("/",b2,sep="")}

  if(is.null(lower.range)){b1=0}
  if(is.null(upper.range)){b2=0}

  geneNameControl=1

  data.folder=dirname(file)
  positions=length(strsplit(basename(file),"\\.")[[1]])
  matrixNameC=strsplit(basename(file),"\\.")[[1]]
  matrixName=paste(matrixNameC[seq(1,positions-1)],collapse="")
  format=strsplit(basename(basename(file)),"\\.")[[1]][positions]
  separator2="tab"

  #updating rownames structure
   tmp <- read.table(file, sep="\t", header=T, row.names=1)
   tmp.n <- rownames(tmp)
   if(length(grep("-miR-", tmp.n)) > 0){
     tmp.n1 <- strsplit(tmp.n, "-")
     tmp.ensembl <- sapply(tmp.n1, function(x){
       paste(x[2:length(x)], collapse="-")
     })
     tmp.symbol <- sapply(tmp.n1, function(x){
         paste(x[2:length(x)],collapse='.')
     })
   }else{
       tmp.n <- strsplit(tmp.n, ":")
       tmp.ensembl <- sapply(tmp.n, function(x)x[2])
       tmp.symbol <- sapply(tmp.n, function(x)x[1])
   }
   rownames(tmp) <- paste(tmp.ensembl, tmp.symbol, sep=":")
   #updating samples names
   tmp.c <- names(tmp)
   tmp.c <- strsplit(tmp.c, "_")
   tmp.samples <- sapply(tmp.c, function(x)x[1])
   names(tmp) <- tmp.samples
   write.table(tmp, paste(data.folder, "heatmap.txt", sep="/"), sep="\t", col.names=NA)



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
  #preprocess matrix and copying files


system(paste("cp ",data.folder,"/heatmap.txt ",scrat_tmp.folder,sep=""))

  #executing the docker job
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/heatmapbase4seq Rscript /home/main.R heatmap txt tab ",status, " ", b1," ", b2, sep="")

resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
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


  #Copy result folder
  cat("Copying Result Folder")
#  system(paste("cp -r ",scrat_tmp.folder,"/",matrixName,"_heatmap.pdf ",data.folder,"/",sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
