#' @title A function to handle a indrop V2 single cell data
#' @description This function executes a docker that produces as output the sinngle cell counts from V2 indrop single cell sequencing
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the path of the scratch folder
#' @param fastq.folder, a character string indicating the folder where input data are located and where output will be written
#' @param index.folder, a character string indicating the folder where transcriptome index was created with salmonIndex.
#' @param sample.name, the name to be associated to the fastq files, e.g. C2 for C2_S2_L001_R1_001.fastq.gz, IMPORTANT input fastq should have the format SAMPLENAME_Sx_L00y_Rz_001.fastq.gz, where x is an integer, y is an integer, z is 1 or 2
#' @param split.affixes, the string separating SAMPLENAME for the Rz_001.fastq.gz
#' @param bowtie.index.prefix, the prefix name of the bowtie index
#' @param M, Ignore reads with more than M alignments, after filtering on distance from transcript end.
#' @param U, Ignore counts from UMI that should be split among more than U genes.
#' @param D, Maximal distance from transcript end, NOT INCLUDING THE POLYA TAIL.
#' @param low.complexity.mask, low complexity mask False or True
#' @param umi.threshold, the minimal number of UMI to consider a gene present
#' @author Raffaele Calogero and Riccardo Panero, raffaele.calogero [at] unito [dot] it, Bioinformatics and Genomics unit, University of Torino Italy
#'
#' @examples
#' \dontrun{
#' system("wget 130.192.119.59/public/testMm_S0_L001_R1_001.fastq.gz")
#' system("wget 130.192.119.59/public/testMm_S0_L001_R2_001.fastq.gz")
#' library(docker4seq)
#' #running indropCounts
#' indropCounts(group="docker", scratch.folder="/data/scratch", fastq.folder=getwd(),
#'        index.folder="/data/genomes/mm10indrop", sample.name="testMm", split.affixes="S0_L001",
#'        bowtie.index.prefix="Mus_musculus.GRCm38.85.index", M=10, U=2, D=400, low.complexity.mask="False", umi.threshold=5)
#' }
#'
#' @export
indropCounts <- function(group=c("sudo","docker"), scratch.folder, fastq.folder, index.folder, sample.name, split.affixes, bowtie.index.prefix, M=10, U=2, D=400, low.complexity.mask=c("False", "True"), umi.threshold=5){

  #testing if docker is running
  test <- dockerTest()
  if(!test){
    cat("\nERROR: Docker seems not to be installed in your system\n")
    return()
  }
  #storing the position of the home folder
  home <- getwd()
  #running time 1
  ptm <- proc.time()
  #setting the data.folder as working folder
  if (!file.exists(fastq.folder)){
    cat(paste("\nIt seems that the ",fastq.folder, " folder does not exist\n"))
    return(2)
  }
  setwd(fastq.folder)

  #FastQC
  fastqc(group="docker", data.folder=fastq.folder)
  #

  #check  if scratch folder exist
  if (!file.exists(scratch.folder)){
    cat(paste("\nIt seems that the ",scratch.folder, " folder does not exist\n"))
    return(3)
  }
  tmp.folder <- gsub(":","-",gsub(" ","-",date()))
  scrat_tmp.folder=file.path(scratch.folder, tmp.folder)
  writeLines(scrat_tmp.folder,paste(fastq.folder,"/tempFolderID", sep=""))
  project.folder <- scrat_tmp.folder
  cat("\ncreating a folder in scratch folder\n")
  dir.create(file.path(scrat_tmp.folder))
  dir.create(paste(file.path(scrat_tmp.folder), "/input", sep=""))
  input.folder <- paste(file.path(scrat_tmp.folder), "/input", sep="")
  dir.create(paste(file.path(scrat_tmp.folder), "/output", sep=""))
  oputput.folder <- paste(file.path(scrat_tmp.folder), "/output", sep="")
  dir <- dir()
  dir <- dir[grep(".fastq.gz", dir)]
  cat("\ncopying \n")
  if(length(dir)==0){
    cat(paste("It seems that in ", fastq.folder, "there are not fastq.gz files"))
    return(1)
  }
  system(paste("chmod 777 -R", file.path(scrat_tmp.folder)))
  for(i in dir){
      system(paste("cp ",fastq.folder,"/",i, " ",paste(file.path(scrat_tmp.folder), "/input", sep=""),"/",i, sep=""))
  }

  yaml.file=paste(path.package(package="docker4seq"),"extras/indrop.yaml",sep="/")
  system(paste("cp ",yaml.file," ", file.path(scrat_tmp.folder),sep=""))
  system(paste("chmod 777 -R", file.path(scrat_tmp.folder)))
  setwd(scrat_tmp.folder)

  #edit yaml
  yaml <- readLines("indrop.yaml")
  project_name <- yaml[grep("project_name", yaml)]
  project_name <- sub("CRISPR", tmp.folder, project_name)
  yaml[grep("project_name", yaml)] <- project_name

  project_dir <- yaml[grep("project_dir", yaml)]
  project_dir <- sub("/sto2/labcamargo/Documents/single_cell/CRISPR_single_cell_9Nov17/inDrops/", "/data/scratch", project_dir)
  yaml[grep("project_dir", yaml)] <- project_dir

  sample_name <- yaml[grep("  - name :", yaml)]
  sample_name <- sub("CRISPR", sample.name, sample_name)
  yaml[grep("  - name :", yaml)] <- sample_name

  input_dir <- yaml[grep("    dir :", yaml)]
  input_dir <- sub("/sto2/labcamargo/Documents/single_cell/CRISPR_single_cell_9Nov17/basespace/171004_M00620_0217_000000000-BFWPC_FASTQ", "/data/scratch/input", input_dir)
  yaml[grep("    dir :", yaml)] <- input_dir

  split_affixes <- yaml[grep("    split_affixes :", yaml)]
  split_affixes <- sub("S1_L001", split.affixes, split_affixes)
  yaml[grep("    split_affixes :", yaml)] <- split_affixes

  library_name <- yaml[grep("library_name:", yaml)]
  library_name <- gsub("Sample1", sample.name, library_name)
  yaml[grep("library_name:", yaml)] <- library_name

  bowtie_index <- yaml[grep("bowtie_index :", yaml)]
  bowtie_index <- gsub("/sto2/labcamargo/Documents/bowtie_index/mm10/Mus_musculus.GRCm38.85.index", paste("/index/",bowtie.index.prefix, sep=""), bowtie_index)
  yaml[grep("bowtie_index :", yaml)] <- bowtie_index

  #UMI parameters
  m <- yaml[grep("    m : 10 #Ignore reads with more than M alignments, after filtering on distance from transcript end.", yaml)]
  m <- sub("10", M, m)
  yaml[grep("    m : 10 #Ignore reads with more than M alignments, after filtering on distance from transcript end.", yaml)] <- m

  u <- yaml[grep("    u : 2 #Ignore counts from UMI that should be split among more than U genes.", yaml)]
  u <- sub("2", U, u)
  yaml[grep("    u : 2 #Ignore counts from UMI that should be split among more than U genes.", yaml)] <- u

  d <- yaml[grep("    d : 400 #Maximal distance from transcript end, NOT INCLUDING THE POLYA TAIL", yaml)]
  d <- sub("400", D, d)
  yaml[grep("    d : 400 #Maximal distance from transcript end, NOT INCLUDING THE POLYA TAIL", yaml)] <- d

  #outout params
  low_complexity_mask <- yaml[grep("    low_complexity_mask: False", yaml)]
  low_complexity_mask <- sub("False", low.complexity.mask, low_complexity_mask)
  yaml[grep("    low_complexity_mask: False", yaml)] <- low_complexity_mask


  zz <- file("indrop.yaml", "w")
  writeLines(yaml, zz)
  close(zz)
  #

  system(paste("chmod 777 -R", file.path(scrat_tmp.folder)))

  cat("\nsetting as working dir the scratch folder and running  docker container\n")
  setwd(fastq.folder)

  #saving running params
  zz <- file("indrop.yaml", "w")
  writeLines(yaml, zz)
  close(zz)

  

  params <- paste("--cidfile ",fastq.folder,"/dockerID -v ", project.folder,":/data/scratch -v ",index.folder,":/index -d docker.io/repbioinfo/indrop.2017.01 sh /bin/indrop.sh ", sep="")
  resultRun <- runDocker(group=group,container="docker.io/repbioinfo/indrop.2017.01", params=params)
 
  if(resultRun==0){
    cat("\n inDrop analysis is finished\n")
    system(paste("cp -R ", project.folder, "/", sample.name, " ", fastq.folder, sep=""))
    system(paste("cp -R ", project.folder, "/output ", fastq.folder, sep=""))
  }


  setwd(fastq.folder)

  #output stat
  dir <- dir(sample.name)
  dir <- dir[grep("counts.tsv.gz$",dir)]
  system(paste("gzip -d ./", sample.name,"/", dir, sep=""))
  counts <- read.table(paste("./", sample.name,"/", sub(".gz$","", dir), sep=""), header=T, row.names = 1, stringsAsFactors = F)
  counts <- t(counts)
  write.table(counts, "counts.txt", sep="\t")
  system(paste("rm ./", sample.name,"/", sub(".gz$","", dir), sep=""))

  cells.counts <- apply(counts, 2, sum)
  genes.cell <- apply(counts, 2, function(x){
    length(which(x > umi.threshold))
  })

  jpeg(paste("counts_stats","_",sample.name, "_", umi.threshold, ".jpg", sep=""))
  plot(log10(cells.counts+1), log10(genes.cell+1), pch=19, xlab="log10(cell counts)", ylab="log10(# of detected genes)", cex=0.5)
  dev.off()


  #running time 2
  ptm <- proc.time() - ptm
  dir <- dir(fastq.folder)
  dir <- dir[grep("run.info",dir)]
  if(length(dir)>0){
    con <- file("run.info", "r")
    tmp.run <- readLines(con)
    close(con)
    tmp.run[length(tmp.run)+1] <- paste("inDrop user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("inDrop system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("inDrop elapsed run time mins ",ptm[3]/60, sep="")
    writeLines(tmp.run,"run.info")
  }else{
    tmp.run <- NULL
    tmp.run[1] <- paste("inDrop user run time mins ",ptm[1]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("inDrop system run time mins ",ptm[2]/60, sep="")
    tmp.run[length(tmp.run)+1] <- paste("inDrop elapsed run time mins ",ptm[3]/60, sep="")

    writeLines(tmp.run,"run.info")
  }

  #saving log and removing docker container
  container.id <- readLines(paste(fastq.folder,"/dockerID", sep=""), warn = FALSE)
  system(paste("docker logs ", substr(container.id,1,12), " &> ",fastq.folder,"/inDrop_", substr(container.id,1,12),".log", sep=""))
  system(paste("docker rm ", container.id, sep=""))

  cat("\n\nRemoving the temporary file ....\n")
  system("rm -fR dockerID")
  system("rm  -fR tempFolderID")
  system(paste("rm -fR ", project.folder, sep=""))
  system(paste("cp ",paste(path.package(package="docker4seq"),"containers/containers.txt",sep="/")," ",fastq.folder, sep=""))
  setwd(home)

}
