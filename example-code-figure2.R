#
#	This code reproduces the analysis shown in Figure 2 of the mzAccess paper.
#


# Load the mzAccess R package
library(mzAccessR)

# Use the mzAccess test service
SetupServer(ServerURL = "http://charon.ad.cmm.se:6060/MzAccess/TestService.asmx?WSDL")


#
# Figure 2A
#

# Coordinates for a full-length chromatogram (16 minute LC run)
mz0 <- 148.0604342; dMz <- 0.002; rtMin <- 0; rtMax <- 16;
chrom <- GetChromatogram("Thermo_QE_standardmix", mz0-dMz, mz0+dMz, rtMin, rtMax)

# Plot the result
plot(chrom, type="l")


#
# Figure 2B
#

# Coordinates for the glutamate peak observed in Figure 2A
mz0 <- 148.0604342; dMz <- 0.002; d13C <- 1.0033548378;
rtMin <- 8; rtMax <- 9;

# Get the unlabeled cell data (left panel in Figure 2B)
chrom <- GetChromatogramArray(
	rep("Thermo_QE_cells_72h_UL_1", 2),
	c(mz0-dMz, mz0 + 5*d13C - dMz),
	c(mz0+dMz, mz0 + 5*d13C + dMz),
	rep(rtMin, 2), rep(rtMax, 2))

# Plot the chromatogram
plot(chrom[[1]], type = "l")
lines(chrom[[2]], col="red")

# Get the labeled cell data (right panel in Figure 2B)
chrom <- GetChromatogramArray(
	rep("Thermo_QE_cells_72h_LA_1", 2),
	c(mz0-dMz, mz0 + 5*d13C - dMz),
	c(mz0+dMz, mz0 + 5*d13C + dMz),
	rep(rtMin, 2), rep(rtMax, 2))

# Plot the chromatogram
plot(chrom[[2]], col="red", type = "l")
lines(chrom[[1]])


#
# Figure 2C
#   
   
# Coordinates for mass isotopomers of interest at the glutamate peak apex
mzMin <- 149.050; mzMax <- 149.070; rtPeak <- 8.35;

# Get spectra in profile mode
spectra <- list(
	GetSpectrumByRT("Thermo_QE_cells_72h_UL_1", mzMin, mzMax, rtPeak, Profile = TRUE),
	GetSpectrumByRT("Thermo_QE_cells_72h_LA_1", mzMin, mzMax, rtPeak, Profile = TRUE))

# Plot the spectra
plot(spectra[[1]], type = "l")
lines(spectra[[2]], col="red")


#
# Figure 2D
#

# Retrieve MZ-RT area data in profile mode, with m/z range as above
mzMin <- 149.050; mzMax <- 149.070; rtMin <- 8.2; rtMax <- 8.5;
area <- GetArea("Thermo_QE_cells_72h_UL_1", mzMin, mzMax, rtMin, rtMax, Profile = TRUE) 

# Since the (MZ, RT, Intensity) values do not form a regular grid,
# we need do to some interpolation before plotting, here using the akima package 
library(akima)
akimaData <- interp(area$Mass, area$RT, area$Intensity, duplicate = "median",
            xo=seq(min(area$Mass), max(area$Mass), length=300),
            yo=seq(min(area$RT), max(area$RT), length=300));
akimaData[[3]][is.na(akimaData[[3]])] <-0

# Fancy 3D plot (this needs the plot3D package)
library(plot3D)
persp3D(akimaData[[1]], akimaData[[2]], akimaData[[3]],
	lighting = TRUE, theta = -150, phi = 20,
	col="gray", xlab="\n\nMass", ylab="\n\nRT", zlab="\n\nIntensity", ticktype="detailed", r=20)


#
# Figure 2E
#

# Coordinates for the bilirubin peak in plasma
mzMin <- 582.3; mzMax <- 583.3; rtMin <- 11.8; rtMax <- 12.5;

# get chromatogram from this region
chrom <- GetChromatogram("Agilent_QTOF_plasma_5", mzMin,mzMax, rtMin, rtMax)
# plot chromatogram
plot(aspline(chrom), type = "l", xlab="RT", ylab="Intensity")

# get fragmentation events from this region
fragInfo <- GetFragmentationEvents("Agilent_QTOF_plasma_5", mzMin, mzMax, rtMin, rtMax)
# plot fragmentation events on top of chromatogram
points(aspline(chrom$RT, chrom$Intensity, xout = fragInfo$RT), col = "red", bg = "red")


#
# Figure 2F
#

# set mz range, get scan number from fragmentation events (above)
mzMin <- 50; mzMax <- 400; scanNumber <- fragInfo$ScanNumber[1];

# retrieve MS/MS spectrum
spectrum <- GetSpectrumByScanNumber("Agilent_QTOF_plasma_5", mzMin, mzMax, scanNumber)
plot(spectrum, type="l")

