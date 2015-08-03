\name{camtrapR-package}
\alias{camtrapR-package}
\alias{camtrapR}
\docType{package}
\title{
Overview of the functions in the camtrapR package
}
\description{
This package provides a streamlined workflow for processing data generated in camera trap-based wildlife studies and prepares input for further analyses.
It suggests a simple data structure and provides functions for managing digital camera trap photographs, generating record databases, maps of species richness and species detections, species activity diagrams and detection/non-detection matrices for occupancy analyses, e.g. in the \pkg{\link[unmarked]{unmarked}} package. Further, basic survey statistics are computed. The function build on one another in a logical sequence.
Apart from species identification, which is achieved by moving images into species directories, and the creation of a table holding basic information about camera trap station IDs, locations and trapping periods, no manual input is required.
}

\details{
Images are renamed automatically and stored in a directory structure of camera trap station directories. Within each of these camera trap station directories, species subdirectories contain the species images. Image metadata are extracted from the images in that directory structure using Phil Harvey's Exiftool (available from \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}) and the information is stored in a record database. An adjustable criterion for temporal independence of records can be applied. Maps of species presence and species richness can be generated. Several functions are available for plotting single- and two-species activity patterns.
Information about the camera-specific trapping periods (and periods of malfunction) are summarized into information about camera trap operability. These, together with the record database are used to generate species detection histories. The user has considerable freedom in generating the detection histories; data format, session lengths, and session start times are adjustable. In addition, trapping effort (i.e. active trap nights per station and session) can be computed for use as a observation covariate in occupancy models.
}

\section{Image management workflow}{

The functions in this section build on one another and can be run in sequential order as needed.

\tabular{ll}{
\code{\link{imageRename}} \tab Copy and rename images based on camera trap station ID and creation date \cr
\code{\link{createSpeciesFolders}} \tab Create species directories for species identification \cr
\code{\link{checkSpeciesFolders}} \tab Consistency check on species image identification \cr
\code{\link{appendSpeciesNames}} \tab Add or remove species names from image filenames \cr
}
}

\section{Data extraction}{

These function use the directory structure built above (Section 'Image management workflow') and a table containing basic information about camera traps and/or stations (IDs, location, trapping period).

\tabular{ll}{
\code{\link{recordDatabase}} \tab Create a species record database from camera trap images \cr
\code{\link{detectionMaps}} \tab Generate maps of species richness and species presence by station \cr
\code{\link{cameraOperation}} \tab Create a camera operability matrix \cr
\code{\link{detectionHistory}} \tab Species detection histories for occupancy analyses \cr
\code{\link{surveyReport}} \tab Create a report about camera trap surveys and species detections \cr
}
}

\section{Species activity plots}{

These plots are generated from the record database.

\tabular{ll}{
\code{\link{activityHistogram}} \tab Single-species diel activity histograms \cr
\code{\link{activityDensity}} \tab Single-species diel activity kernel density estimation plots \cr
\code{\link{activityRadial}} \tab Single-species diel activity radial plot \cr
\code{\link{activityOverlap}} \tab Two-species diel activity overlap plots and estimates \cr
}
}

\section{Additional functions}{

\tabular{ll}{
\code{\link{exiftoolPath}}\tab Add a directory to PATH temporarily (for exiftool.exe under Windows)\cr
\code{\link{exifTagNames}} \tab Extract Exif metadata tag names from jpg images \cr
\code{\link{timeShiftImages}} \tab Apply time shifts to jpg image metadata \cr
\code{\link{getSpeciesImages}} \tab Copy all images of a species to a new directory \cr
}
}

\section{Sample data}{

\tabular{ll}{
\code{\link{camtraps}}\tab  Sample camera trap station information table \cr
\code{\link{recordDatabaseSample}} \tab Sample species record database \cr
\code{\link{timeShiftTable}} \tab Sample camera trap time shift information (for function \code{\link{timeShiftImages}}) \cr
}
}

\author{
Juergen Niedballa

Maintainer:Juergen Niedballa <niedballa@izw-berlin.de>

}
\references{
Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12. \cr
Mike Meredith and Martin Ridout (2014). overlap: Estimates of coefficient of overlapping for animal activity patterns. R package version 0.2.4. \url{http://CRAN.R-project.org/package=overlap} \cr
Phil Harvey's Exiftool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ } \cr
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
\pkg{\link[overlap]{overlap}}
\pkg{\link[unmarked]{unmarked}}
\pkg{\link[plotrix]{plotrix}}
}

