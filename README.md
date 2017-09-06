# docker4seq
A collection of functions to execute NGS computing demanding applications, e.g reads mapping and counting, wrapped in docker containers.
To install it you can use use **devtools**:
```
install.packages("devtools")
library(devtools)
install_github("kendomaniac/docker4seq", ref="master")
```


##Requirements
You need to have docker installed on your machine, for more info see this document:
https://docs.docker.com/engine/installation/. 
**docker4seq** package is expected to run on 64 bits linux machine with at least 4 cores and 32 Gb RAM.
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


In case you wish to use MAC OS or windows you need to install https://www.docker.com/products/docker-toolbox and run within the Docker Quickstart Terminal.app

##Diclaimer:
docker4seq developers have no liability for any use of docker4seq functions, including without limitation, any loss of data, incorrect results, or any costs, liabilities, or damages that result from use of docker4seq. 


