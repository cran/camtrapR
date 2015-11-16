\name{timeShiftImages}
\alias{timeShiftImages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply time shifts to JPEG image metadata
}
\description{
Change the values of digital timestamps in image metadata using Exiftool. If date/time of images were set incorrectly, they can be easily corrected in batch mode for further analyses. Please, always make a backup of your data before using this function to avoid data loss or damage.
}
\usage{
timeShiftImages (inDir,
  timeShiftTable,
  stationCol,
  cameraCol,
  hasCameraSubfolders,
  timeShiftColumn,
  timeShiftSignColumn,
  undo = FALSE
)
}

\arguments{
  \item{inDir}{
   character. Directory containing station directories with images
}
  \item{timeShiftTable}{
   data.frame containing information about station-/camera-specific time shifts. 
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{timeShiftTable}
}
  \item{cameraCol}{
  character. name of the column specifying Camera ID in \code{timeShiftTable} (optional)
}
  \item{hasCameraSubfolders}{
  logical. Apply time shifts to camera-subdirectories only? 
}
  \item{timeShiftColumn}{
  character. The name of the column containing time shift values in \code{timeShiftTable}
}
  \item{timeShiftSignColumn}{
  character. The name of the column with the direction of time shifts in \code{timeShiftTable}. Can only be "-" or "+".
}
  \item{undo}{
  logical. Undo changes and restore the original images? Please be careful, this deletes any edited images if \code{TRUE}
}
}
\details{

\code{timeShiftTable} is a data frame with columns for station ID, camera ID (optional), time shift value and direction of time shift.
Images in \code{inDir} are expected to be sorted into station directories. If \code{hasCameraSubfolders = TRUE}, the function will look for camera subdirectories of the station directories and only apply time shifts to the camera subdirectories specified by \code{CameraCol} in \code{timeShiftTable}. If \code{hasCameraSubfolders = FALSE}, shifts will be applied to the whole station directory (including potential subdirectories).

The values of \code{timeShiftColumn} should adhere to the following pattern: "Y:M:D H:M:S", i.e. "1:0:0 0:0:0" is a shift of exactly 1 year and "0:0:0 12:10:01" 12 hours and 10 minutes and 1 second. \code{timeShiftSignColumn} signifies the direction of the time shift. "+" moves image dates into the future (i.e. the image date lagged behind the actual date) and "-" moves image dates back (if the image dates were ahead of actual time).

Exiftool stores the original images as \code{.JPG_original} files in the original file location. By setting \code{undo = TRUE}, any JPG files in the directories specified by \code{timeShiftTable} will be deleted and the original JPEGs will be restored from the JPG_original files. Please make a backup before using \code{undo}.
}
\value{
 A \code{data.frame} containing the information about the processed directories and the number of images.
}
\references{
\url{http://www.sno.phy.queensu.ca/~phil/exiftool/Shift.html}
}
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% }
\examples{

\dontrun{

# copy sample images to another location (so we don't mess around in the package directory)
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = getwd(), recursive = TRUE)
wd_images_ID_copy <- paste(getwd(), "sample_images", sep = "/")

data(timeShiftTable)


timeshift_run <- timeShiftImages(inDir                = wd_images_ID_copy,
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "Station",
                                 hasCameraSubfolders  = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign",
                                 undo                 = FALSE
)


timeshift_undo <- timeShiftImages(inDir               = wd_images_ID_copy,
                                  timeShiftTable      = timeShiftTable,
                                  stationCol          = "Station",
                                  hasCameraSubfolders = FALSE,
                                  timeShiftColumn     = "timeshift",
                                  timeShiftSignColumn = "sign",
                                  undo                = TRUE
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
