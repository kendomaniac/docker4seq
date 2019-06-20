#' @title Function to execute Cutadapt on RNA-Seq reads
#' @description This function executes the docker container  by running CutAdapt algorithm a the input RNA-Seq dataset to remove the sequencing adapters.
#' @param group, a character string. Two options: \code{"sudo"} or \code{"docker"}, depending to which group the user belongs
#' @param data.folder, a character string indicating where fastq files are located
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param threads, a number indicating the number of cores to be used from the application
#' @param adapter.type, a character string. Two options: \code{"ILLUMINA"} or \code{"NEB"}, depending to which miRNA library prep was used: ILLUMINA or NEB
#' @author Nicola Licheri and Giulio Ferrero
#' @return The fastq files of the trimmed sequencing reads
#' @examples
#'\dontrun{
#' ...
#' }
#' @export


cutadapt <- function(group = c("sudo", "docker"), scratch.folder, data.folder,
    adapter.type = c("ILLUMINA", "NEB"), threads = 1) {

    #running time 1
    ptm <- proc.time()

    scratch.folder <- normalizePath(scratch.folder)
    data.folder <- normalizePath(data.folder)

    if (adapter.type %in% c("ILLUMINA", "NEB") == FALSE) {
        cat(paste("\nInvalid adapter type:", mode))
        system("echo 3 > ExitStatusFile 2>&1")
        return(3)
    }

    if (!file.exists(data.folder)){
        cat(paste("\nIt seems that the", data.folder, "folder does not exist\n"))
        system("echo 2 > ExitStatusFile 2>&1")
        return(2)
    }

    # storing the position of the home folder
    home <- getwd()
    setwd(data.folder)

    params <- paste(
        "--cidfile", paste0(data.folder, "/dockerID"),
        "-v", paste0(data.folder, ":/data"),
        "-d docker.io/repbioinfo/cutadapt.2019.01 /bin/bash /bin/cutadapt.sh", adapter.type, threads
    )

    resultRun <- runDocker(group=group, params=params)


    #running time 2
    ptm <- proc.time() - ptm
    dir <- dir(data.folder)
    dir <- dir[grep("run.info",dir)]
    if(length(dir)>0) {
      con <- file("run.info", "r")
      tmp.run <- readLines(con)
      close(con)
      tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
      writeLines(tmp.run,"run.info")
    }
    else {
      tmp.run <- NULL
      tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")

      writeLines(tmp.run,"run.info")
    }

    #saving log and removing docker container
    container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
    system(paste0("docker logs ", substr(container.id,1,12), " &> ",data.folder,"/cutadapt_", substr(container.id,1,12),".log"))
    system(paste("docker rm", container.id))
    # removing temporary files
    cat("\n\nRemoving the temporary file ....\n")
    system("rm -fR out.info")
    system("rm -fR dockerID")
    system(paste("cp", paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/"), data.folder))
    setwd(home)

}
