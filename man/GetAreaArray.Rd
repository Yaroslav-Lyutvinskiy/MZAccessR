% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WSInterface.R
\name{GetAreaArray}
\alias{GetAreaArray}
\title{Get Array of LC-MS Area signals}
\usage{
GetAreaArray(FileNames, MZLows, MZHighs, RTLows, RTHighs, Cache = TRUE,
  Profile = FALSE)
}
\arguments{
\item{FileNames}{- List of names of original raw mass spectrometry file.}

\item{Cache}{- If TRUE data will be loaded from fast access cache, if FALSE - from original raw files}

\item{Profile}{- If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode}

\item{MZLow}{- List of minimum m/z values for LC-MS areas requested}

\item{MZHigh}{- List of maximum m/z values for LC-MS areas requested}

\item{RTLow}{- List of minimum retention time values for LC-MS areas requested}

\item{RTHigh}{- List of maximum retention time values for LC-MS areas requested}
}
\value{
List of data frames. Each n-th data frame contains Mass, Retention Time and Intensities for all non-zero signals for LC-MS area, specified
 by n-th members of incoming arrays.
}
\description{
Batch analog of GetArea function.
Gets List of data frames where each data frame contains all non-zero signals for one of specified LC-MS areas.
Equal length of incoming lists is assumed.
Corresponds to mzAccess web-service API function GetAreasArray
}
\examples{
Coordinates for the glutamate peak observed in Figure 2A in reference paper
mz0 <- 148.0604342; dMz <- 0.002; d13C <- 1.0033548378;rtMin <- 8; rtMax <- 9;
Get MZ/RT areas for 1-st and 6-th isotopes
Areas <- GetAreaArray(
 rep("Thermo_QE_cells_72h_LA_1", 2),
 c(mz0-dMz, mz0 + 5*d13C - dMz),
 c(mz0+dMz, mz0 + 5*d13C + dMz),
 rep(rtMin, 2), rep(rtMax, 2))
}
