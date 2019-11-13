#' @title A function allowing the identification of differentially expressed genes if multiple groups are provided.
#' @description This function executes in a docker edgeR for the identification of differentially expressed genes in bulk RNAseq. IMPORTANT the filename shoould not have any '.' in the name unless .txt
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param file, a character string indicating the path of the file, with counts.table name and extension included
#' @param logFC.threshold, minimal logFC present in at least one of the comparisons with respect to reference covariate
#' @param FDR.threshold, minimal FDR present in at least one of the comparisons with respect to reference covariate
#' @param logCPM.threshold,  minimal average abundance
#' @param plot, TRUE if differentially expressed genes are represented in a plot.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#'
#' @examples
#' \dontrun{
#'     #running deDetection
#'     anovaLike(group="docker", file=paste(getwd(),"annotated_lorenz_buettner_counts_noSymb.txt", sep="/"),
#'        logFC.threshold=1, FDR.threshold=0.05, logCPM.threshold=4)
#' }
#'
#' @export
anovaLike <- function(group=c("sudo","docker"), file, logFC.threshold=1, FDR.threshold, logCPM.threshold=4, plot=c(TRUE, FALSE)){

  data.folder=dirname(file)
  positions=length(strsplit(basename(file),"\\.")[[1]])
  matrixNameC=strsplit(basename(file),"\\.")[[1]]
  matrixName=paste(matrixNameC[seq(1,positions-1)],collapse="")
  format=strsplit(basename(basename(file)),"\\.")[[1]][positions]
  file.type=format
  counts.table=paste(matrixName, file.type, sep=".")


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
  system("echo 0 >& ExitStatusFile")

  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 >& ExitStatusFile")
    setwd(home)
    return(10)
  }

  #executing the docker job
  params <- paste("--cidfile ",data.folder,"/dockerID -v ", data.folder, ":/data -d docker.io/repbioinfo/desc.2018.02 Rscript /bin/debulk.R ", counts.table, " ", file.type, sep="")
  resultRun <- runDocker(group=group, params=params)

  #waiting for the end of the container work
  if(resultRun==0){
    cat("\nDifferential expression analysis is finished\n")
  }
  system(paste("mv DE_", counts.table, " ANOVAlike_", counts.table, sep=""))
  tmp0 <- read.table(paste("ANOVAlike_", counts.table, sep=""), sep="\t", header=T, row.names=1)
  tmp0.names <- rownames(tmp0)
  tmp0.names1 <- strsplit(tmp0.names, ":")
  tmp0.names2 <- sapply(tmp0.names1, function(x)x[1])
  zz <- file("bkg2GO.txt", "w")
  writeLines(tmp0.names2, con=zz)
  close(zz)
  max0.logfc.tmp <- apply(tmp0[,grep("logFC", names(tmp0))], 1, function(x) unique(x[which(abs(x)== max(abs(x)))]))
  max0.logfc <- sapply(max0.logfc.tmp, function(x)as.numeric(x[[1]]))

  tmp <- tmp0[which(tmp0$logCPM >= logCPM.threshold),]
  max.logfc <- apply(tmp[,grep("logFC", names(tmp))], 1, function(x) max(abs(x)))
  tmp <- tmp[which(max.logfc >= logFC.threshold),]
  tmp <- tmp[which(tmp$FDR <= FDR.threshold),]
  max1.logfc.tmp <- apply(tmp[,grep("logFC", names(tmp))], 1, function(x){
    x[which(abs(x)== max(abs(x)))]
  })
  max1.logfc <- sapply(max1.logfc.tmp, function(x)as.numeric(x[[1]]))
  if(plot){
    pdf("filtered_ANOVAlike.pdf")
      plot(tmp0$logCPM, max0.logfc, xlab="log2CPM", ylab="log2FC", type="n")
      points(tmp$logCPM, max1.logfc, pch=19, cex=0.5, col="red")
      points(tmp0$logCPM, max0.logfc, pch=".", col="black")
      abline(h=0, col="black", lty=2)
    dev.off()
  }

write.table(tmp, paste("filtered_ANOVAlike_", counts.table, sep=""), sep="\t", col.names=NA)
tmp.names <- rownames(tmp)
tmp.names1 <- strsplit(tmp.names, ":")
tmp.names2 <- sapply(tmp.names1, function(x)x[1])
zz <- file("genesGO.txt", "w")
writeLines(tmp.names2, con=zz)
close(zz)

  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(data.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("Anova-like user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("Anova-like system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("Anova-like elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("Anova-like run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("Anova-like system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("Anova-like elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))
  #removing temporary folder
  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  setwd(home)
}
