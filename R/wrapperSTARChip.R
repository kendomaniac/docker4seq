#' @title Wrapper function for circRNAs prediction using STARChip
#' @description This function calls sequentially the docker containers for FASTQC, STAR, and STARChip to predict the list of circRNAs starting from the raw RNA-Seq reads
#'
#' @param group, a character string. Two options: sudo or docker, depending to which group the user belongs
#' @param scratch.folder, a character string indicating the scratch folder where docker container will be mounted
#' @param genome.folder, a character string indicating the folder where the indexed reference genome for STAR is located.
#' @param samples.folder, the folder where are located all the subfolders of the samples processed with starChimeric
#' @param threads, a number indicating the number of cores to be used from the application
#' @param chimSegmentMin, is a positive integer indicating the minimal length of the overlap of a read to the chimeric element
#' @param chimJunctionOverhangMin, is a positive integer indicating the minimum overhang for a chimeric junction
#' @param reads.cutoff, Integer. Minimum number of reads crossing the circRNA backsplice required.
#' @param min.subject.limit, Integer. Minimum number of individuals with readsCutoff reads required to carry forward a circRNA for analysis
#' @param do.splice, true false. The splices within the circRNA be detected and reported. Linear splices are searched within each circRNA in each individual. Any linear splice with >= 60\% of the read count of the cRNA is considered a splice within the circRNA. Two files are then created, .consensus with most common splice pattern, and .allvariants with all reported splice patterns.
#' @param cpm.cutoff, Float. Reads counts are loaded into R and log2(CountsPerMillion) is calculated using the limma package. With cpmCutoff > 0, circRNAs with log2(CPM) below this value will be filtered from this analysis
#' @param subjectCPM.cutoff, Integer. See above. This value is the lower limit for number of individuals required to have the circRNAs expressed at a value higher than cpmCutoff.
#' @param annotation, true/false. circRNAs are provided with gene annotations
#' @author Nicola Licheri, nicola [dot] licheri [at] unito [dot] it, University of Turin, Italy
#' 
#' @return 1. Count matrices : raw cRNA backsplice counts: circRNA.cutoff[readthreshold]reads.[subjectthreshold]ind.countmatrix log2CPM of above: norm_log2_counts_circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples.txt Maximum Linear Splices at Circular Loci: rawdata/linear.[readthreshold]reads.[subjectthreshold]ind.sjmax 2. Info about each circRNA:  Consensus Information about Internal Splicing: Circs[reads].[subjects].spliced.consensus Complete Gene Annotation: circRNA.[readthreshold]reads.[subjectthreshold]ind.annotated Consise Gene Annotation + Splice Type:  circRNA.[readthreshold]reads.[subjectthreshold]ind.genes 3. Images: PCA plots: circRNA.[readthreshold]reads.[subjectthreshold]ind.0cpm_0samples_variance_PCA.pdf Heatmap: circRNA.[readthreshold]reads.[subjectthreshold]ind.heatmap.pdf
#' @examples
#' \dontrun{
#'
#'     #retrieve the example data
#'     system("wget https://github.com/carlo-deintinis/circhunter/archive/master.zip") #retrieve the data of the indexed genome (chromosome 21 of hg38 human genome assembly)
#'     system("unzip master.zip")
#'     system("unzip ./circhunter-master/CircHunter/data/hg38.chr21.fa.zip")
#'     system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_1.fastq.gz") #retrieve the RNA-Seq data
#'     system("wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/SRR582/001/SRR5824251/SRR5824251_2.fastq.gz") #retrieve the RNA-Seq data
#'
#'     #running the wrapperSTARChip function
#' wrapperSTARChip(group = "docker", scratch.folder="/data/scratch", genome.folder="./circhunter-master/CircHunter/data/", samples.folder=getwd(), threads = 8, chimSegmentMin = 20, chimJunctionOverhangMin = 15, reads.cutoff = 5, min.subject.limit = 1, do.splice = FALSE, cpm.cutoff = 0, subjectCPM.cutoff = 0, annotation = FALSE) 
#'
#' }
#' @export


wrapperSTARChip <- function(group=c("sudo", "docker"), 
    #I/O parameters + #threads 
    scratch.folder, genome.folder, samples.folder, threads, 
    # STARChimeric parameters
    chimSegmentMin, chimJunctionOverhangMin, 
    # STARChipCircle parameters
    reads.cutoff, min.subject.limit, do.splice, cpm.cutoff, subjectCPM.cutoff, annotation) {

    home <- getwd()

    scratch.folder <- normalizePath(scratch.folder)
    genome.folder <- normalizePath(genome.folder)
    samples.folder <- normalizePath(samples.folder)

    starChipIndex(group, genome.folder)

    for (sample in list.dirs(path = samples.folder, full.names = TRUE)) {
        if (sample != samples.folder) {
            ## Quality control using FASTQC 
            fastqc(group = group, data.folder = sample)
            # moving FASTQC output in a subfolder 
            fastqc_files <- list.files(sample, full.names = TRUE)
            fastqc_files <- fastqc_files[grep("_tmp_fastqc", fastqc_files)]
            fastqc_output.folder <- file.path(sample, "fastqc_output")
            if (!dir.exists(fastqc_output.folder)) {
                dir.create(fastqc_output.folder)
            }
            system(paste("mv", paste(fastqc_files, collapse = " "), fastqc_output.folder))

            ## Chimeric transcripts detection 
            starChimeric(group = group, fastq.folder = sample, scratch.folder = scratch.folder, 
                genome.folder = genome.folder, threads = threads, chimSegmentMin = chimSegmentMin, 
                chimJunctionOverhangMin = chimJunctionOverhangMin)
        }
    }
    
    ## circRNA prediction 
    starchipCircle(group=group, scratch.folder=scratch.folder, genome.folder=genome.folder, 
        samples.folder=samples.folder, reads.cutoff=reads.cutoff, min.subject.limit=min.subject.limit, 
        threads=threads, do.splice=do.splice, cpm.cutoff=cpm.cutoff, 
        subjectCPM.cutoff=subjectCPM.cutoff, annotation=annotation)
    
    setwd(home)
}