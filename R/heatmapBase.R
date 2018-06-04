#' @title hfc, heatmap for clustering
#' @description This function generate heatmap and other plot based on clustering and on a specific gene list
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param data.folder, a character string indicating the folder where input data are located and where output will be written
#' @param matrixName, counts table name. Matrix data file must be in data.folder. The file MUST contain RAW counts, without any modification, such as log transformation, normalizatio etc. 
#' @param format, matrix count format, "csv", "txt"
#' @param separator, separator used in count file, e.g. '\\t', ','
#' @param geneNameControl, 0 if the matrix has gene name without ENSEMBL code; 1 if the gene names are formatted as : ENSMUSG00000000001:Gnai3. If the gene names is only the ENSEMBL ID please run SCannoByGtf, before start using this function.
#' @param status, 0 if is raw count, 1 otherwise
#' @author Luca Alessandri , alessandri [dot] luca1991 [at] gmail [dot] com, University of Torino
#'
#' @return stability plot for each nCluster,two files with score information for each cell for each permutation. 
#' @examples
#'\dontrun{
#'heatmapBase("sudo",scratch.folder,data.folder,"random_10000_filtered_annotated_lorenz_naive_penta2_0","csv",",",1,1)# 
#'}
#' @export
heatmapBase <- function(group=c("sudo","docker"), scratch.folder, data.folder,
                        matrixName, format, separator, geneNameControl=1, status){


  
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

  
 
if(separator=="\t"){
separator2="tab"
}else{separator2=separator}


if(geneNameControl==0){
system(paste("cp ",data.folder,"/",matrixName,".",format," ",data.folder,"/",matrixName,"_old.",format,sep=""))
mainMatrix=read.table(paste(data.folder,"/",matrixName,".",format,sep=""),header=TRUE,row.names=1,sep=separator2)
rownames(mainMatrix)=paste(seq(1,nrow(mainMatrix)),":",rownames(mainMatrix),sep="")
write.table(mainMatrix,paste(data.folder,"/",matrixName,".",format,sep=""),sep=separator2,col.names=NA)
}


system(paste("cp ",data.folder,"/",matrixName,".",format," ",scrat_tmp.folder,sep=""))

  #executing the docker job
    params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/rcaloger/heatmapbase4seq Rscript /home/main.R ",matrixName," ",format," ",separator," ",status, sep="")
 
resultRun <- runDocker(group=group, params=params)
  
  #waiting for the end of the container work
  if(resultRun==0){
    #system(paste("cp ", scrat_tmp.folder, "/* ", data.folder, sep=""))
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
  system(paste("cp -r ",scrat_tmp.folder,"/",matrixName,"_heatmap.pdf ",data.folder,"/",sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system(paste("rm -R ",scrat_tmp.folder))
  system("rm -fR out.info")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  #system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
