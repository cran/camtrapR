\name{recordDatabase}
\alias{recordDatabase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a species record database from camera trap images
}
\description{
Generates a record database from a folder structure of camera trap images (station/species). Within each station directory, all images need to be sorted into species subdirectories.
}
\usage{
recordDatabase(inDir, 
  includeCameras = FALSE, 
  exclude, 
  minDeltaTime = 0, 
  timeZone, 
  writecsv = FALSE, 
  outDir,
  metadataTags
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
  character. Directory containing station directories with renamed and identified images in species subdirectories (e.g. inDir/StationA/SpeciesA)
}
  \item{includeCameras}{
  logical. Should function try to derive camera ID from image names? Requires images renamed with \code{\link{imageRename}}.
}
  \item{exclude}{
  character vector of species names to be excluded from the record database
}
  \item{minDeltaTime}{
  numeric. time difference between observation of the same species at the same station to be considered independent (in minutes)
}
  \item{timeZone}{
  character. \code{timeZone} must be an argument of \code{\link[base]{OlsonNames}}
}
  \item{writecsv}{
  logical. Should the record database be saved as a .csv?
}
  \item{outDir}{
  character. Directory to save csv to. If NULL and \code{writecsv = TRUE}, recordTable will be written to \code{inDir}
}
  \item{metadataTags}{
  character. additional camera model-specific metadata tags to be extracted. 
}
}
\details{

\code{minDeltaTime} is a criterion for temporal independence of records of a species at the same station/location. Setting it to 0 will make the function return all records. 
\code{exclude} can be used to exclude "species" directories containing irrelavant images (e.g. "team", "blank", "unidentified").
Many digital images contain  Exif metadata tags such as "AmbientTemperature" or "MoonPhase" that can be extracted if specified in \code{metadataTags}. Because these are manufacturer-specific and not standardized, function \code{\link{exifTagNames}} provides a vector of all available tag names. Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2, ...)}. The metadata tags thus extracted may be used as covariates in occupancy modelling.

}
\value{
A data frame containing species records and additional information about stations, date, time and (optionally) further metadata.
}
\references{
Phil Harvey's Exiftool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ }
}
\author{
Juergen Niedballa
}
\note{
The results of a number of other function will depend on the output of this function (namely on the arguments \code{exclude} for excluding species and \code{minDeltaTime} for temporal independence):

\tabular{l}{
\code{\link{detectionMaps}} \cr
\code{\link{detectionHistory}} \cr
\code{\link{activityHistogram}} \cr
\code{\link{activityDensity}} \cr
\code{\link{activityRadial}} \cr
\code{\link{activityOverlap}} \cr
\code{\link{activityHistogram}} \cr
\code{\link{surveyReport}} \cr
}
}

\examples{
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
minDeltaT <- 60   # minutes between independent observations

if (Sys.which("exiftool") != ""){        # only run these examples if Exiftool is available

rec.db1 <- recordDatabase(inDir = wd_images_ID,
  minDeltaTime = minDeltaT,
  writecsv = FALSE,
  metadataTags = c("Model", "Make")
)

rec.db2 <- recordDatabase(inDir = wd_images_ID,
  minDeltaTime = minDeltaT,
  exclude = "NO_ID",
  writecsv = FALSE,
  metadataTags = c("Model", "Make")
)

any(rec.db1$Species == "NO_ID")
any(rec.db2$Species == "NO_ID")

} else {                                # show function output if Exiftool is not available
print("Exiftool is not available. Cannot test function")
data(recordDatabaseSample)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
