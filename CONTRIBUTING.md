## Steps required to submit a new application/workflow:

- Edit the skeleton.R function and the ubuntu docker image (docker.io/repbioinfo/ubuntu) to create the new application.

    + Please have a look at: [Controlling jobs in a docker image, a brief tutorial](http://rpubs.com/rcaloger/300960).
    
- Create a public docker repository for the docker image, e.g. at docker.com.
    
- Create a workflow.Rmd vignette using RStudio and publish it via RStudio. As example of a vignette see [docker4seq vignette](http://rpubs.com/rcaloger/293366).
    
- Once the docker image, the function(s) and vignette are ready please contact info@reproducible-bioinformatics.org. 

    + We will test and incorporate the code in docker4seq package. 
    
    + Mantainers will be responsable of the maintainance of their application(s).


If you are interested to participate to the project or if you need more information please contact info@reproducible-bioinformatics.org
