# docker4seq
A collection of functions to execute NGS computing demanding applications, e.g reads mapping and counting, wrapped in docker containers.
To install it you can use use **devtools**:
```
install.packages("devtools")
library(devtools)
install_github("kendomaniac/docker4seq", ref="master")
```


## Requirements
You need to have docker installed on your machine, for more info see this document:
https://docs.docker.com/engine/installation/. 
**docker4seq** package is expected to run on 64 bits linux machine with at least 4 cores and 32 Gb RAM, if mapping will be done with STAR. In case mapping is done with Salmon, only 16 Gb RAM are needed.
A scratch folder should be present, e.g. /data/scratch and it should be writable from everybody:
```
chmod 777 /data/scratch
```
The functions in docker4seq package require that user is sudo or part of a docker group.
See the following document for more info:
https://docs.docker.com/engine/installation/linux/ubuntulinux/#/manage-docker-as-a-non-root-user

**IMPORTANT** The first time *docker4seq* is installed the **downloadContainers** needs to be executed  to download to the local repository the containers that are needed for the use of *docker4seq*

More info on the functionalities of the package are available at: [**docker4seq/4SeqGUI vignette**](http://rpubs.com/rcaloger/293366)

- **docker4seq/4SeqGUI Video Tutorials:**

    + [HowTo run a full RNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhTi9MYMysNI3O4fR0wt46D)
    
    + [HowTo run a full miRNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRix-Er5unoze68qE8vW46s-)
    
    + [HowTo run a full ChIPseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhqjyPBGkDRTf0wNi4Y9Va2)


**testSeqbox**
In *docker4seq* library is now present the function *testSeqbox*, allowing to check if  the software required for docker4seq functionalities is properly installed. Check *?testSeqbox* to see how to use it.


## Workflows compliance with Sandve rules:

- The **whole transcriptome workflow**, embedding annotatingByGtf.R, demultiplexing.R, experimentPower.R, fastqc.R, filterCounts.R, pca.R, rnaseqCounts.R, rsemAnnotate.R, rsemStar.R, rsemstarIndex.R, salmonAnnotation.R, salmonCounts.R, salmonIndex.R, samples2experiment.R, sampleSize.R, skewer.R, wrapperDeseq2.R, wrapperSalmon.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules. 

- The **miRNAs analysis workflow**, embedding demultiplexing.R, experimentPower.R, fastqc.R, filterCounts.R, pca.R, mirnaCounts.R, mirnaCovar.R, sampleSize.R, wrapperDeseq2.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules. 

- The **Xenome module**, which allows to discriminate between mouse and human reads in patient derived xenograft DNA/RNA sequenced samples, embedding xenome.R and xenomeIndex.R, fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules.

- The **inDrop single cell module**, which allows the single cell UMI counting generated with inDrop single cell sequencing technology, embedding indropCounts.R and demultiplexing.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules. 

- The **Platypus module**, which allows haplotype-based variant calling for next generation sequence data, embedding demultiplexing.R, bwa.R and platypus.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285)

- The **Circular RNA identification module**, which allows the identification circRNAs, embedding starChipIndex.R, starChimeric.R and starchipCircle.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285)

- The **ChIPseq workflow** embedding demultiplexing.R, chipseq.R, chipseqCounts.R, bwa.R, bwaIndexUcsc.R does not satisfy Rule 4 (Version Control All Custom Scripts) because it download for annotation the latest version of ENSEMBL annotation. Thus, annotation executed at different time might differ because of the changes in the ENSEMBL downoaded information. We are working to fix this issue, expected fixing Q3 2018.



## Diclaimer:
docker4seq developers have no liability for any use of docker4seq functions, including without limitation, any loss of data, incorrect results, or any costs, liabilities, or damages that result from use of docker4seq. 


