##Installation instruction

## mzAccessR depends on SSOAP package for SOAP Server connection. 
## SSOAP enlisted as Bioconductor extras package. 
## Therefore, to get SSOAP package we need to get access to Bioconductor installation system:
source("https://bioconductor.org/biocLite.R")
## And then install SSOAP package:
biocLite("SSOAP")
## As mzAccessR located on GitHub we need devtools suite:
install.packages("devtools")
library(devtools)
## And then use install_github function to setup mzAccessR package
install_github("Yaroslav-Lyutvinskiy/MZAccessR")
## Intialize library:
library(mzAccessR)
## Setup Server – by default it is KI mzAccess webserver: 
SetupServer()
## And then use the functions of package on your demand, for example, like that:
GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
