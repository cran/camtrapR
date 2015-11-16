\name{recordDatabase}
\alias{recordDatabase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a species record database from camera trap images
}
\description{
Generates a record database from camera trap images. Images must be sorted into station directories at least. The function can read species identification from a directory structure (Station/Species or Station/Camera/Species) or from image metadata tags.
}
\usage{
recordDatabase(inDir,
  hasSpeciesFolders = TRUE,
  cameraID,
  camerasIndependent,
  exclude,
  minDeltaTime = 0,
  timeZone,
  stationCol,
  writecsv = FALSE,
  outDir,
  customMetadataTags,
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag,
  additionalMetadataTags
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
  character. Directory containing station directories with renamed and identified images in species subdirectories (e.g. inDir/StationA/SpeciesA).
}
  \item{hasSpeciesFolders}{
  logical. Are species images sorted into species directories? If not, species identification will be derived from metadata tags.
}
  \item{cameraID}{
  character. Where should the function look for camera IDs: 'filename', 'directory'. 'filename' requires images renames with \code{\link{imageRename}}. 'directory' requires a camera subdirectory within station directories (station/camera/species)). Can be missing.
}
  \item{camerasIndependent}{
  logical. If \code{TRUE}, species records are considered to be independent between cameras at a station.
}
  \item{exclude}{
  character. Vector of species names to be excluded from the record database
}
  \item{minDeltaTime}{
  integer. Time difference between observation of the same species at the same station to be considered independent (in minutes)
}
  \item{timeZone}{
  character. Must be an argument of \code{\link[base]{OlsonNames}}
}
  \item{stationCol}{
  character. Name of the camera trap station column.
}
  \item{writecsv}{
  logical. Should the record database be saved as a .csv?
}
  \item{outDir}{
  character. Directory to save csv to. If NULL and \code{writecsv = TRUE}, recordTable will be written to \code{inDir}.
}
  \item{customMetadataTags}{
  logical. Should function try to extract custom metadata tags from images (from metadata tag "HierarchicalSubject").
}
  \item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":". 
}
  \item{metadataSpeciesTag}{
  character. In custom image metadata, the species ID tag name.
}
  \item{additionalMetadataTags}{
  character. Additional camera model-specific metadata tags to be extracted.
}
}
\details{
The function can handle a number of different ways of storing images, and supports species identification by moving images into species directories as well as metadata tagging. In every case, images need to be stored into station directories. If images are identified by moving them into species directories, a camera directory is optional: "Station/Species/XY.JPG" or "Station/Camera/Species/XY.JPG". Likewise, if images are identified using metadata tagging, a camera directory can be used optionally: "Station/XY.JPG" or "Station/Camera/XY.JPG". In the latter case, \code{metadataSpeciesTag} specifies the metadata tag group name that contains species identification tags.
\code{minDeltaTime} is a criterion for temporal independence of records of a species at the same station/location. Setting it to 0 will make the function return all records. \code{camerasIndependent} defines if the cameras at a station are to be considered independent (e.g. \code{FALSE} if both cameras face each other and possibly \code{TRUE} if they face different trails).
\code{exclude} can be used to exclude "species" directories containing irrelevant images (e.g. "team", "blank", "unidentified"). \code{stationCol} can be set to match the station column name in the camera trap station table (see \code{\link{camtraps}}).
Many digital images contain  Exif metadata tags such as "AmbientTemperature" or "MoonPhase" that can be extracted if specified in \code{metadataTags}. Because these are manufacturer-specific and not standardized, function \code{\link{exifTagNames}} provides a vector of all available tag names. Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2, ...)}. The metadata tags thus extracted may be used as covariates in modelling species distributions.
\code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam and images tagged in Adobe Bridge / Lightroom with the default settings.

}
\value{
A data frame containing species records and additional information about stations, date, time and (optionally) further metadata.
}

\section{Warning }{
Custom image metadata tags must be written to the images. The function cannot read tags from .xmp sidecar files. Make sure you set the preferences accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There, make sure "Write from sidecar files" is unchecked.
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

if (Sys.which("exiftool") != ""){        # only run these examples if Exiftool is available

rec.db1 <- recordDatabase(inDir                  = wd_images_ID,
                          minDeltaTime           = 60,
                          writecsv               = FALSE,
                          additionalMetadataTags = c("Model", "Make")
)

rec.db2 <- recordDatabase(inDir                  = wd_images_ID,
                          minDeltaTime           = 60,
                          exclude                = "NO_ID",
                          writecsv               = FALSE,
                          additionalMetadataTags = c("Model", "Make")
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