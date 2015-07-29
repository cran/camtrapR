\docType{data}
\name{camtraps}
\alias{camtraps}
\title{Sample camera trap station information}
\format{A data frame with 3 rows and 7 variables}
\usage{
data(camtraps)
}
\description{
General camera trap station information
}
\details{
This is a general example of a \code{data.frame} that holds information about camera trap stations, e.g. setup and retrieval dates and coordinates. If more than 1 camera was set up at a station (often 2), a camera ID column can be added, with camera-specific information instead of station-specific information.
If cameras malfunctioned repeatedly, additional pairs of problem columns can be added, e.g. "Problem2from" and "Problem2to" etc..

The variables are as follows:

\itemize{
 \item Station.   Camera trap station ID
 \item utm_y.   y coordinate of station (northing)
 \item utm_x.   x coordinate of station (easting)
 \item Setup_date.   camera trap setup date
 \item Retrieval_date.   camera trap retrieval date
 \item Problem1from.   date of beginning of camera malfunction
 \item Problem1to.   date of end of camera malfunction
 }
}

\note{
The coordinates can be in the units of any coordinate system. UTM was chosen as an example, but it could be latlong or anything else, too. When using the function \code{\link{detectionMaps}}, set arguments \code{Xcol} and \code{Ycol} to the appropriate names. 
}
\keyword{datasets}

