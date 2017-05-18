# *******************************************************************************
#   Copyright 2015-2017 Yaroslav Lyutvinskiy <Yaroslav.Lyutvinskiy@ki.se> and
# Roland Nilsson <Roland.Nilsson@ki.se>
#
#   Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.using System;
#
# *******************************************************************************

# package installation is tested for R v.3.3
# mzAccessR depends on SSOAP package for SOAP Server connection."
# SSOAP enlisted as Bioconductor extras package."
# Therefore, to get SSOAP package we need to get access to Bioconductor installation system:"
source("https://bioconductor.org/biocLite.R")
# And then install SSOAP package:"
biocLite("SSOAP")
# As mzAccessR located on GitHub we need devtools suite:"
install.packages("devtools")
library(devtools)
# And then use install_github function to setup mzAccessR package
install_github("Yaroslav-Lyutvinskiy/MZAccessR")
# Intialize library:
library(mzAccessR)
# Setup Server ? by default it is KI mzAccess webserver:
# There will be some warnings from SSOAP level, that is considered as normal
SetupServer()
# And then use the functions of package on your demand, for example, like that:
GetChromatogram("Thermo_QE_cells_72h_LA_3",148.0584342,148.0624342,0,16)
