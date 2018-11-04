#' @title Downloading the full set of genes information from HGNC
#' @description This function retrieves the full table of curated genes annotations
#'
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param data.folder, a character string indicating where downloaded data will be stored
#' @param url, the url to download the full dataset
#' @param type, PUBMED or OMIM
#' @author Raffaele Calogero
#'
#' @return one file files: gene_with_protein_product.txt
#' @examples
#'\dontrun{
#'     getInfo(group="docker",data.folder=getwd(),
#'     url="ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/locus_types/gene_with_protein_product.txt", type="pubmed")
#'
#' }
#' @export
getInfo <- function(group=c("sudo","docker"), data.folder=getwd(), url, type=c("pubmed", "omim")){
  home <- getwd()
  setwd(data.folder)
  dir <- dir()
  if(length(grep("gene_with_protein_product.txt", dir)) > 0){
    file.rename("gene_with_protein_product.txt", paste(gsub(":","-",gsub(" ","-",date())), "gene_with_protein_product.txt", sep="_"))
  }

  #initialize status
  system("echo 0 >& ExitStatusFile")

  #running time 1
  ptm <- proc.time()
  #running time 1
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    system("echo 10 >& ExitStatusFile")
    setwd(home)
    return(10)
  }
  params <- paste("--cidfile ",data.folder,"/dockerID -v ",data.folder,":/data -d docker.io/repbioinfo/genes.2018.01 sh /bin/getinfo.sh ", url, " ", type, sep="")
  resultRun <- runDocker(group=group, params=params)

  if(resultRun==0){
    cat("\nData download is finished\n")
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
  system(paste("docker logs ", container.id, " >& ", "getInfo_",substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
  system(paste("rm  -f ",data.folder,"/dockerID", sep=""))
  setwd(home)
}



