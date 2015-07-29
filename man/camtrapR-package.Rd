\name{camtrapR-package}
\alias{camtrapR-package}
\alias{camtrapR}
\docType{package}
\title{
Overview of the functions in the camtrapR package
}
\description{
This package provides a streamlined workflow for the processing and analysis of data generated in camera trap-based wildlife studies.
It suggests a simple data structure and provides functions for managing digital camera trap photographs, generates record databases, maps and detection/non-detection matrices for occupancy analyses, e.g. in the \pkg{\link[unmarked]{unmarked}} package. The function build on one another in a logical sequence. 
Apart from species identification, which is achieved by moving images into species directories, and the creation of a table holding basic information about camera trap station IDs, locations and trapping periods, no manual input is required.
}

\details{
Image metadata are extracted using Phil Harvey's Exiftool (available from  \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}). 
From the folder structure thus set up, a record database can be created with an adjustable criterion for temporal independence of records. Maps of species presence and species richness can be generated as well as simple survey statistics. 
Several functions are available for plotting single- and two-species activity patterns.
Information about the camera-specific trapping periods are summarized into information about camera trap operability. These together with the record database are used to generate species detection histories. The user has considerable freedom in generating the detection histories; format, session lengths, and session start times are adjustable.
}


\section{Image management workflow}{

The functions in this section build on one another and can be run in sequential order as needed.

\tabular{ll}{
\code{\link{exiftoolPath}}\tab Add directory containing Exiftool to PATH \cr
\code{\link{TimeShiftImages}} \tab Apply time shifts to image metadata \cr
\code{\link{imageRename}} \tab Copy and rename images based on station ID, camera ID, date and time\cr
\code{\link{createSpeciesFolders}} \tab Create directories for species identification\cr
\code{\link{checkSpeciesFolders}} \tab Check if images were identified correctly \cr
\code{\link{appendSpeciesNames}} \tab Add or remove species names from images \cr
\code{\link{exifTagNames}} \tab Extract Exif metadata tag names \cr
\code{\link{recordDatabase}} \tab Create a species record database from camera trap images\cr
\code{\link{detectionMaps}} \tab Generate maps of species richness and species presence by station \cr
\code{\link{cameraOperation}} \tab Compute a camera operability matrix \cr
\code{\link{detectionHistory}} \tab Generate species detection histories \cr
\code{\link{surveyReport}} \tab Generate information about trapping periods and species detections \cr
}
}

\section{Species activity plots}{

\tabular{ll}{
\code{\link{activityHistogram}} \tab Single-species diel activity histograms \cr
\code{\link{activityDensity}} \tab Single-species diel activity kernel density estimation plots \cr
\code{\link{activityRadial}} \tab Single-species diel activity radial plot \cr
\code{\link{activityOverlap}} \tab Two-species diel activity overlap plots and estimates\cr
}
}

\section{Additional functions}{
\tabular{ll}{
\code{\link{getSpeciesImages}} \tab Fetch and save all images of a species \cr
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

