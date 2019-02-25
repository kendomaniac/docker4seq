# Reproducible Bioinformatics Community

The aim of Reproducible Bioinformatics project is the creation of easy to use Bioinformatics workflows that fullfill the following roles ([Sandve et al. PLoS Comp Biol. 2013](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285)):

    1 For Every Result, Keep Track of How It Was Produced
    2 Avoid Manual Data Manipulation Steps
    3 Archive the Exact Versions of All External Programs Used
    4 Version Control All Custom Scripts
    5 Record All Intermediate Results, When Possible in Standardized Formats
    6 For Analyses That Include Randomness, Note Underlying Random Seeds
    7 Always Store Raw Data behind Plots
    8 Generate Hierarchical Analysis Output, Allowing Layers of Increasing Detail to Be Inspected
    9 Connect Textual Statements to Underlying Results
    10 Provide Public Access to Scripts, Runs, and Results

Reproducible Bioinformatics is a non-profit and open-source project.

We are a group of Bioinformaticians interested to simplify the use of bioinformatics tools to Biologists w/wo scripting ability. At the same time we are interested in providing robust and reproducible workflows.

For this reason we have developed the docker4seq package.

At the present time a total of three workflows are available in the stable version of docker4seq package (more info below in the text):

    - RNAseq workflow
    - miRNAseq workflow
    - ChIPseq workflow

Under development are:

    - PDX workflow: variants calling in patient derived xenograft (PDX) from RNAseq and EXOMEseq data
    - Single cell analysis workflow
    - Metagenomics workflow

All workflows are controlled by a set of R fuctions, part of docker4seq package, and the algorithms used are all encapsulated into Docker images and stored at docker.io/repbioinfo repository.

More info on docker4seq: [docker4seq web page](https://kendomaniac.github.io/docker4seq/index.html)


### How to be part of the Reproducible Bioinformatics Project community


Any bioinformatician interested to embed specific applications in the available workflows or interested to develop a new workflow is requested to embed the application(s) in a docker image, save it in a public repository and configure one or more R functions that can be used to interact with the docker image. The module/workflow needs to fullfil at least the first 6 Sandve's rules.

Steps required to submit a new application/workflow:

- Edit the [skeleton.R](https://github.com/kendomaniac/docker4seq/blob/devel/R/skeleton.R) function and the ubuntu docker image (docker.io/repbioinfo/ubuntu) to create the new application.


    + Please have a look at: [Controlling jobs in a docker image, a brief tutorial](https://kendomaniac.github.io/docker4seq/articles/skeleton.html).


- Create a public docker repository for the docker image, e.g. at [docker.com](http://docker.com).

- Create a workflow.Rmd vignette using RStudio and publish it via RStudio. As example of a vignette see [docker4seq vignette](https://kendomaniac.github.io/docker4seq/).

- Once the docker image, the function(s) and vignette are ready please fill this [submission form](http://goo.gl/bb42EN).

    + If your module/workflow passes the validation analysis, you will be inserted as developer of the docker4seq package, and you will add your module and vignette to the docker4seq github repository.

    + If your module will not satisfy some of the validation points we will help you in fixing the issues and having your module compliant to the requirements of the Reproducible-bioinformatics Project. Mantainers will be responsable of the maintainance of their application(s).


### docker4seq

**docker4seq** is registed with RRID SCR_017006 at [*SciCrunch*](scicrunch.org). *docker4seq** is part of  [*Elixir bio.tools*](https://bio.tools/).

A collection of functions to execute NGS computing demanding applications, e.g reads mapping and counting, wrapped in docker containers.
To install it you can use use **devtools**:

```
install.packages("devtools")
library(devtools)
install_github("kendomaniac/docker4seq", ref="master")
```

#### Requirements
You need to have docker installed on your machine, for more info see this document:
https://docs.docker.com/engine/installation/.
**docker4seq** package is expected to run on 64 bits linux machine with at least 4 cores.  32 Gb RAM are required only if mapping will be done with STAR. In case mapping is done with Salmon, only 16 Gb RAM are needed.
A scratch folder should be present, e.g. /data/scratch and it should be writable from everybody:

```
chmod 777 /data/scratch
```

The functions in docker4seq package require that user is sudo or part of a docker group.
See the following document for more info:
https://docs.docker.com/install/linux/linux-postinstall/

**IMPORTANT** The first time *docker4seq* is installed the **downloadContainers** needs to be executed  to download to the local repository the containers that are needed for the use of *docker4seq*

More info on the functionalities of the package are available at: [**docker4seq/4SeqGUI vignette**](https://kendomaniac.github.io/docker4seq/index.html)

- **docker4seq/4SeqGUI Video Tutorials:**

    + [HowTo run a full RNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhTi9MYMysNI3O4fR0wt46D)

    + [HowTo run a full miRNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRix-Er5unoze68qE8vW46s-)

    + [HowTo run a full ChIPseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhqjyPBGkDRTf0wNi4Y9Va2)


**testSeqbox**
In *docker4seq* library is now present the function *testSeqbox*, allowing to check if  the software required for docker4seq functionalities is properly installed. Check *?testSeqbox* to see how to use it.


#### Workflows compliance with Sandve rules:

- The **whole transcriptome workflow**, embedding annotatingByGtf.R, demultiplexing.R, experimentPower.R, fastqc.R, filterCounts.R, pca.R, rnaseqCounts.R, rsemAnnotate.R, rsemStar.R, rsemstarIndex.R, salmonAnnotation.R, salmonCounts.R, salmonIndex.R, samples2experiment.R, sampleSize.R, skewer.R, wrapperDeseq2.R, wrapperSalmon.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules.

- The **miRNAs analysis workflow**, embedding demultiplexing.R, experimentPower.R, fastqc.R, filterCounts.R, pca.R, mirnaCounts.R, mirnaCovar.R, sampleSize.R, wrapperDeseq2.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules.

- The **Xenome module**, which allows to discriminate between mouse and human reads in patient derived xenograft DNA/RNA sequenced samples, embedding xenome.R and xenomeIndex.R, fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules.

- The **inDrop single cell module**, which allows the single cell UMI counting generated with inDrop single cell sequencing technology, embedding indropCounts.R and demultiplexing.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) reproducibility rules.

- The **Platypus module**, which allows haplotype-based variant calling for next generation sequence data, embedding demultiplexing.R, bwa.R and platypus.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285)

- The **Circular RNA identification module**, which allows the identification circRNAs, embedding starChipIndex.R, starChimeric.R and starchipCircle.R fullfils all [Sandve](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285)

- The **ChIPseq workflow** embedding demultiplexing.R, chipseq.R, chipseqCounts.R, bwa.R, bwaIndexUcsc.R does not satisfy Rule 4 (Version Control All Custom Scripts) because it download for annotation the latest version of ENSEMBL annotation. Thus, annotation executed at different time might differ because of the changes in the ENSEMBL downoaded information. We are working to fix this issue, expected fixing Q3 2018. Not all intermediate results are available as part of the final results (Sandve rule 5), expected fixing Q3 2018.

#### Diclaimer:
docker4seq developers have no liability for any use of docker4seq functions, including without limitation, any loss of data, incorrect results, or any costs, liabilities, or damages that result from use of docker4seq.

### 4SeqGUI Project

[4SeqGUI](https://github.com/mbeccuti/4SeqGUI) is the GUI that can control the docker4seq functionalities. It represents the graphical interface used in SeqBox project (see below).

Video tutorials for 4SeqGUI:

[HowTo run a full RNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhTi9MYMysNI3O4fR0wt46D)

[HowTo run a full miRNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRix-Er5unoze68qE8vW46s-)

[HowTo run a full ChIPseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhqjyPBGkDRTf0wNi4Y9Va2)


### The SeqBox Project

Short reads sequencing technology has been used for more than a decade now. However, the analysis of RNAseq and ChIPseq data is still computational demanding and the simple access to raw data does not guarantee results reproducibility between laboratories. To address these two aspects, we developed SeqBox, a cheap, efficient and reproducible RNAseq/ChIPseq hardware/software solution based on NUC6I7KYK mini-PC (an Intel consumer game computer with a fast processor and a high performance SSD disk), and Docker container platform. In SeqBox the analysis of RNAseq and ChIPseq data is supported by a friendly GUI. This allows access to fast and reproducible analyses also to scientists with/without scripting experience.

More info on SeqBox characteristics and cost are available at [www.seqbox.com](http://www.seqbox.com)


**IMPORTANT** The first time *docker4seq* is installed the **downloadContainers** needs to be executed  to download to the local repository the containers that are needed for the use of *docker4seq*

More info on the functionalities of the package are available at: [docker4seq/4seqGUI vignette](http://rpubs.com/rcaloger/279935)

- **docker4seq/4SeqGUI Video Tutorials:**

    + [HowTo run a full RNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhTi9MYMysNI3O4fR0wt46D)

    + [HowTo run a full miRNAseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRix-Er5unoze68qE8vW46s-)

    + [HowTo run a full ChIPseq analysis](https://www.youtube.com/playlist?list=PLN48SoNXrLRhqjyPBGkDRTf0wNi4Y9Va2)



## Diclaimer:
docker4seq developers have no liability for any use of docker4seq functions, including without limitation, any loss of data, incorrect results, or any costs, liabilities, or damages that result from use of docker4seq.
