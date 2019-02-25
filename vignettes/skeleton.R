## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#  skeleton(group="docker", scratch.folder, data.folder)

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#    #storing the position of the home folder
#    home <- getwd()
#    #running time 1
#    ptm <- proc.time()

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#    #testing if docker is running
#    test <- dockerTest()
#    if(!test){
#      cat("\nERROR: Docker seems not to be installed in your system\n")
#      return()
#    }

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#    #setting the data.folder as working folder
#    if (!file.exists(data.folder)){
#      cat(paste("\nIt seems that the ",data.folder, " folder does not exist\n"))
#      return(2)
#    }
#    setwd(data.folder)

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#    #check  if scratch folder exist
#    if (!file.exists(scratch.folder)){
#      cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
#      return(3)
#    }
#    tmp.folder <- gsub(":","-",gsub(" ","-",date()))
#    scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
#    writeLines(scrat_tmp.folder,paste(data.folder,"/tempFolderID", sep=""))
#    cat("\ncreating a folder in scratch folder\n")
#    dir.create(file.path(scrat_tmp.folder))

## ---- echo=TRUE,eval=FALSE-----------------------------------------------
#    #executing the docker job
#    if(group=="sudo"){
#      params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/repbioinfo/ubuntu sh /bin/skeleton.sh", sep="")
#      resultRun <- runDocker(group="sudo", params=params)
#    }else{
#      params <- paste("--cidfile ",data.folder,"/dockerID -v ",scrat_tmp.folder,":/scratch -v ", data.folder, ":/data -d docker.io/repbioinfo/ubuntu sh /bin/skeleton.sh", sep="")
#      resultRun <- runDocker(group="docker", params=params)
#    }

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#   #when container ends
#   if(resultRun=="false"){
#     #everything is copied to the input folder
#      system(paste("mv ", scrat_tmp.folder,"/* ",data.folder, sep=""))
#       #saving log and removing docker container
#      container.id <- readLines(paste(data.folder,"/dockerID", sep=""), warn = FALSE)
#      system(paste("docker logs ", substr(container.id,1,12), " &> ", substr(container.id,1,12),".log", sep=""))
#      system(paste("docker rm ", container.id, sep=""))
#      #removing temporary folder
#      cat("\n\nRemoving the temporary file ....\n")
#      system(paste("rm -R ",scrat_tmp.folder))
#      system("rm -fR out.info")
#      system("rm -fR dockerID")
#      system("rm  -fR tempFolderID")
#      system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",data.folder, sep=""))
#   }
#  

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#    #running time 2
#    ptm <- proc.time() - ptm
#    dir <- dir(data.folder)
#    dir <- dir[grep("run.info",dir)]
#    if(length(dir)>0){
#      con <- file("run.info", "r")
#      tmp.run <- readLines(con)
#      close(con)
#      tmp.run[length(tmp.run)+1] <- paste("user run time mins ",ptm[1]/60, sep="")
#      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
#      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
#      writeLines(tmp.run,"run.info")
#    }else{
#      tmp.run <- NULL
#      tmp.run[1] <- paste("run time mins ",ptm[1]/60, sep="")
#      tmp.run[length(tmp.run)+1] <- paste("system run time mins ",ptm[2]/60, sep="")
#      tmp.run[length(tmp.run)+1] <- paste("elapsed run time mins ",ptm[3]/60, sep="")
#      writeLines(tmp.run,"run.info")
#    }

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#    setwd(home)

