\name{imageRename}
\alias{imageRename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Copy and rename images based on camera trap station ID and creation date
}
\description{
The function copies raw images into a new location where they can be identified. In the process, images are renamed with camera trap station ID, camera ID (optional), creation date and a numeric identifier for images taken within one minute of each other at a given station. Station ID and camera ID are derived from the raw image directory structure. The creation date is extracted from image metadata using Exiftool.
}
\usage{
imageRename(inDir, 
  outDir, 
  hasCameraSubfolders,
  keepCameraSubfolders,
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
  \item{keepCameraSubfolders}{
     logical. Should camera directories be preserved as subdirectories of \code{outDir} (e.g. outDir/StationA/CameraA1; outDir/StationA/CameraA2)?
}
  \item{copyImages}{
  logical. Copy images to \code{outDir}?
}
  \item{writecsv}{
  logical. Save a data frame with a summary as a .csv?
}
}
\details{
Setting up the correct raw image directory structure is necessary for running the function successfully. \code{inDir} is the main directory, which must contain subdirectories for each camera trap station (e.g. inDir/StationA). If there was one camera per station, \code{hasCameraSubfolders} can be set to \code{FALSE} and the function will only look for images in the station subdirectories of \code{inDir}. If there was more than one camera at a station, there must be subdirectories for the individual camera traps within the station directories (e.g. inDir/StationA/CameraA1 and inDir/StationA/CameraA2), with argument \code{hasCameraSubfolders} set to \code{TRUE}. Within the camera subdirectories, the directory structure is irrelevant. 
The function then searches for all JPEG images in the Station or Station/Camera subdirectories of \code{inDir}, extracts the creation date from the image metadata using Exiftool and copies the images (with new file names) into \code{outDir}, where it will set up a directory structure based on the camera trap station IDs and, if required by  \code{keepCameraSubfolders = TRUE}, camera IDs. 
Renaming of images follow the following pattern: If \code{hasCameraSubfolders} is TRUE, it is: "StationID__CameraID__Date__Time(Number).JPG", e.g. "StationA__CameraA1__2015-01-31__18-59-59(1).JPG". If \code{hasCameraSubfolders} is FALSE, it is:  "StationID__Date__Time(Number).JPG", e.g. "StationA__2015-01-31__18-59-59(1).JPG". The purpose of the number in parentheses is to prevent assigning identical file names to images taken at the same station (and camera) in the same second, as can happen in if cameras take sequences of images. It is a consecutive number given to all images taken at the same station by the same camera within one minute. The double underscore "__" in the image file names is for splitting and extracting information from file names in other functions (e.g. for retrieving camera IDs in \code{\link{recordDatabase}} if camera subdirectories are not preserved).
After the execution of this function, species are identified by moving image files into species subdirectories of each camera trap station (or station/camera) directory in \code{outDir}. The species subdirectories can be created using function \code{\link{createSpeciesFolders}}.
\code{copyImages} can be set to FALSE to simulate the renaming and check the file names of the renamed images without copying.

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

# because copyImages = FALSE, outDir does not need to be defined
renaming.table <- imageRename(inDir               = wd_images_raw,
                              # outDir            = getwd(),       
                              hasCameraSubfolders = FALSE,
                              copyImages          = FALSE,
                              writecsv            = FALSE
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
  
  # now we have to set outDir because copyImages = TRUE
renaming.table2 <- imageRename(inDir               = wd_images_raw,
                               outDir              = wd_images_raw_renamed,       
                               hasCameraSubfolders = FALSE,
                               copyImages          = TRUE,
                               writecsv            = FALSE
  )
  }
  
  list.files(wd_images_raw_renamed, recursive = TRUE)
  
  }
}