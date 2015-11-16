\name{recordDatabaseIndividual}
\alias{recordDatabaseIndividual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a single-species record database from camera trap images with custom metadata tags
}
\description{
The function generates a single-species record database containing individual IDs for (spatial) capture-recapture analyses. It prepares input for the function \code{\link{spatialDetectionHistory}}.
}
\usage{
recordDatabaseIndividual(inDir,
  hasStationFolders,
  hasIndividualFolders,
  cameraID,
  camerasIndependent,
  minDeltaTime = 0,
  timeZone,
  stationCol,
  writecsv = FALSE,
  outDir,
  customMetadataTags,
  metadataHierarchyDelimitor = "|",
  metadataIDTag,
  additionalMetadataTags
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
  character. Directory containing station directories with renamed and identified images in species subdirectories (e.g. inDir/StationA/SpeciesA).
}
  \item{hasStationFolders}{
  logical. Do station directories have camera subdirectories?
}
  \item{hasIndividualFolders}{
  logical. Are species images sorted into species directories? If not, species identification will be derived from metadata tags.
}
  \item{cameraID}{
  character. Where should the function look for camera IDs: 'filename', 'directory'. 'filename' requires images renames with \code{\link{imageRename}}. 'directory' requires a camera subdirectory within station directories (station/camera/species)). Can be missing.
}
  \item{camerasIndependent}{
  logical. If \code{TRUE}, camera IDs are taken from image file names (requires images renames with function \code{link{imageRename}}) or from the directory structure (assumes directory structure such as: Station/Camera/Species).
}
  \item{minDeltaTime}{
  numeric. time difference between observation of the same species at the same station to be considered independent (in minutes)
}
  \item{timeZone}{
  character. \code{timeZone} must be an argument of \code{\link[base]{OlsonNames}}
}
  \item{stationCol}{
  character. Name of the camera trap station column. "Station" by default.
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
  \item{metadataIDTag}{
  character. In custom image metadata, the individual ID tag name.
}
  \item{additionalMetadataTags}{
  character. additional camera model-specific metadata tags to be extracted.
}
}
\details{
The function can handle a number of different ways of storing images, and supports individual identification by moving images into individual-specific directories as well as metadata tagging. In every case, images need to be stored into a species directory first (e.g. using function \code{\link{getSpeciesImages}}). Station subdirectories are optional. Individuals can then be identified from images by moving them into individual directories ("Species/Station/Individual/XY.JPG" or "Species/Individual/XY.JPG"). Alternatively, individuals can be identified from images using metadata tagging without the need for directories for individuals: "Species/XY.JPG" or "Species/Station/XY.JPG". In that case, \code{metadataIDTag} specifies the metadata tag group name that contains individual identification tags.

\code{minDeltaTime} is a criterion for temporal independence of records of a species at the same station/location. Setting it to 0 will make the function return all records. \code{camerasIndependent} defines if the cameras at a station are to be considered independent (e.g. \code{FALSE} if both cameras face each other and possibly \code{TRUE} if they face different trails).
\code{exclude} can be used to exclude "species" directories containing irrelevant images (e.g. "team", "blank", "unidentified"). \code{stationCol} can be set to match the station column name in the camera trap station table (see \code{\link{camtraps}}).
Many digital images contain  Exif metadata tags such as "AmbientTemperature" or "MoonPhase" that can be extracted if specified in \code{metadataTags}. Because these are manufacturer-specific and not standardized, function \code{\link{exifTagNames}} provides a vector of all available tag names. Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2, ...)}. The metadata tags thus extracted may be used as covariates in occupancy modelling.
\code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam and images tagged in Adobe Bridge / Lightroom with the default settings.

}
\value{
A data frame containing species records and individual IDs and additional information about stations, date, time and (optionally) further metadata.
}

\section{Warning }{
Custom image metadata tags must be written to the images. The function cannot read tags from .xmp sidecar files. Make sure you set the preferences accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There, make sure "Write from sidecar files" is unchecked.
}

\references{
Phil Harvey's Exiftool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}
}

\author{
Juergen Niedballa
}

 \examples{
 
 wd_images_ID <- system.file("pictures/sample_images_tagged/LeopardCat", package = "camtrapR")
 # missing space in species = "LeopardCat" is because of CRAN package policies

 if (Sys.which("exiftool") != ""){        # only run these examples if Exiftool is available

 rec.db.pbe <- recordDatabaseIndividual(inDir                  = wd_images_ID,
                                        minDeltaTime           = 60,
                                        hasStationFolders      = FALSE,
                                        hasIndividualFolders   = FALSE,
                                        camerasIndependent     = FALSE,
                                        writecsv               = FALSE,
                                        customMetadataTags     = TRUE,
                                        metadataIDTag          = "individual",
                                        additionalMetadataTags = c("Model", "Make"),
                                        timeZone               = "Asia/Kuala_Lumpur"
 )

 } else {                                # show function output if Exiftool is not available
 print("Exiftool is not available. Cannot test function")
 data(recordDatabaseIndividualSample)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line