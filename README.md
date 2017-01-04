# docker4seq
A collection of functions to execute NGS computing demanding applications, e.g reads mapping and counting, wrapped in docker containers.
To install it you can use use **devtools**:
```
install.packages("devtools")
library(devtools)
install_github("install_github("kendomaniac/docker4seq")")
```
or you can use **githubinstall**:
```
install.packages("githubinstall")
library(githubinstall)
githubinstall("kendomaniac/docker4seq")
```

##Requirements
You need to have docker installed on your machine, for more info see this document:
https://docs.docker.com/engine/installation/. 
**docker4seq** package is expected to run on 64 bits machine with at least 4 cores and 32 Gb RAM.
A scratch folder should be present, e.g. /data/scratch and it should be writable from everybody:
```
chmod 777 /data/scratch
```
The functions in docker4seq package require that user is sudo or part of a docker group.
See the following document for more info:
https://docs.docker.com/engine/installation/linux/ubuntulinux/#/manage-docker-as-a-non-root-user


