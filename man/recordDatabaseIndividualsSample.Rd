\docType{data}
\name{recordDatabaseIndividualSample}
\alias{recordDatabaseIndividualSample}
\title{Sample single-species record database with custom metadata from camera trap images}
\format{A data frame with 21 rows and 17 variables}
\usage{
data(recordDatabaseIndividualSample)
}
\description{
Sample single-species record database from camera trap images generate with function \code{\link{recordDatabaseIndividual}} from the tagged sample images in the package.
}
\details{


The variables are as follows:

\itemize{
 \item Station.   Camera trap station ID
 \item Species.   Species ID
 \item Individual.   Individual ID
 \item DateTimeOriginal.   Date and time as extracted from image
 \item Date.   record date
 \item Time.   record time of day
 \item delta.time.secs.   time difference to first species record at a station (seconds)
 \item delta.time.mins.   time difference to first species record at a station (minutes)
 \item delta.time.hours.   time difference to first species record at a station (hours)
 \item delta.time.days.   time difference to first species record at a station (days)
 \item Directory.   Image directory
 \item FileName.   image filename
 \item HierarchicalSubject.   content of the HierarchicalSubject image metadata tag
 \item Model.   camera model extracted from image metadata
 \item Make.   camera make extracted from image metadata
 \item metadata_Species.   content of custom image metadata tag "Species" (see HierarchicalSubject)
 \item metadata_individual.   content of custom image metadata tag "individual" (see HierarchicalSubject)
 }
}
\keyword{datasets}

