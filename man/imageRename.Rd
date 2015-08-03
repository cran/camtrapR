\name{imageRename}
\alias{imageRename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Copy and rename images based on camera trap station ID and creation date
}
\description{
The function copies images into a new location and renames them with camera trap station ID, camera ID (optional), creation date and a numeric identifier for images taken within one minute of each other at a given station. Station ID and camera ID are derived from the directory structure. The creation date is extracted from image metadata using Exiftool.
}
\usage{
imageRename(inDir, 
  outDir, 
  hasCameraSubfolders,
  copyImages = FALSE, 
  writecsv = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
     character. Directory containing camera trap station subdirectories (e.g. inDir/StationA)
}
  \item{outDir}{
     character. Directory into which the renamed images will be copied
}
  \item{hasCameraSubfolders}{
     logical. Do the station subdirectories of \code{inDir} have camera-subdirectories  (e.g. inDir/StationA/CameraA1; inDir/StationA/CameraA2)?
}
  \item{copyImages}{
  logical. Copy images to \code{outDir}?
}
  \item{writecsv}{
  logical. Save a data frame with a summary as a .csv?
}
}
\details{
Setting up the correct folder structure is necessary for running the function successfully. \code{inDir} is the main directory, which must contain subdirectories for each camera trap station. If there was one camera per station, \code{hasCameraSubfolders} can be set to \code{FALSE} and the function will only look for images in the station subdirectories of \code{inDir}. If there was more than one camera at a station, there must be subdirectories for the individual camera traps within the station directories and \code{hasCameraSubfolders} must be set to \code{TRUE}. Within the camera subdirectories, the directory structure is irrelevant. 
The function recursively searches for all .jpg images in the subdirectories of \code{inDir}, extracts the creation date from the image metadata using Exiftool and copies the renamed images into \code{outDir}, where it will set up a directory structure based on the camera trap station IDs (not cameras). If \code{hasCameraSubfolders} is TRUE, the renaming pattern is as follows: "StationID__CameraID__Date__Time(Number).JPG", e.g. "StationA__CameraA1__2015-01-31__18-59-59(1).JPG". If \code{hasCameraSubfolders} is FALSE, it is:  "StationID__Date__Time(Number).JPG", e.g. "StationA__2015-01-31__18-59-59(1).JPG". The purpose of the number in parentheses is to prevent assigning identical file names to images taken at the same station (and camera) in the same second, as can happen in if cameras take sequences of images.
After the execution of this function, species are identified by moving image files into species subdirectories of each camera trap station directories in \code{outDir}. The species subdirectories can be created using function \code{\link{createSpeciesFolders}}.
\code{copyImages} can be set to FALSE to simulate the renaming and check the filenames of the renamed images without copying.

}
\value{
A \code{data.frame} with original directories and file names, new directories and file names and an indicator whether images were copied successfully.
}
\references{
Phil Harvey's Exiftool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ }
}
\author{
Juergen Niedballa
}

\examples{

### "trial" run. create a table with file names after renaming, but don't copy images.

# first, find sample image directory in package directory:
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")

if (Sys.which("exiftool") != ""){        # only run this example if Exiftool is available
renaming.table <- imageRename(inDir = wd_images_raw,
 # outDir = getwd(),       # because copyImages = FALSE, outDir does not need to be defined
  hasCameraSubfolders = FALSE,
  copyImages = FALSE,
  writecsv = FALSE
  )
  } else {
  print("Exiftool is not available. Cannot test function")
  }
  
  \dontrun{
  
  # define image directories

  # raw image location
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")      
  # destination for renamed images
wd_images_raw_renamed <- paste(getwd(), "raw_images_renamed", sep = "/")       


  if (Sys.which("exiftool") != ""){        # only run this example if Exiftool is available
renaming.table2 <- imageRename(inDir = wd_images_raw,
  outDir = wd_images_raw_renamed,       # now, copyImages = TRUE, so we have to set outDir
  hasCameraSubfolders = FALSE,
  copyImages = TRUE,
  writecsv = FALSE
  )
  }
  
  list.files(wd_images_raw_renamed, recursive = TRUE)
  
  }

}
