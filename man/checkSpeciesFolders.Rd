\name{checkSpeciesFolders}
\alias{checkSpeciesFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Consistency check on species image identification
}
\description{
This function assesses possible misidentification of species or mistakes in moving images to species directories. Within each station, it assesses whether there are images of a species taken within a given time interval in other species directories. Often, it is unlikely that different species are encountered within a very short time intervals at the same location.
This type of misidentification can arise easily if some images belonging to a sequence of images were accidentally moved into different species directories.
}
\usage{
checkSpeciesFolders(inDir, 
  hasCameraSubfolders,
  maxDeltaTime, 
  excludeSpecies,
  stationsToCheck,
  writecsv = FALSE
)
}

\arguments{
  \item{inDir}{
  character. Directory containing station subdirectories, which in turn contain renamed and identified images in species subdirectories (e.g. inDir/StationA/Species1)
}
  \item{hasCameraSubfolders}{
  logical. Do the station directories in \code{inDir} have camera subdirectories (e.g. inDir/StationA/Camera1/Species1)?
}
  \item{maxDeltaTime}{
  numeric. Maximum time interval between images to be returned (in seconds)
}
  \item{excludeSpecies}{
  character. vector of species directories to exclude from check
}
  \item{stationsToCheck}{
  character. vector of stations to be checked (optionally)
}
  \item{writecsv}{
  logical. Should the resulting data.frame be saved as a .csv?
}
}
\details{
Images may accidentally be moved into wrong directories when identifying species by moving images into directories. Imagine your camera takes 3 shots every time it is triggered, and you move those three images into different species directories. The time difference will be very small (e.g. a few seconds). This function will return all these images for you to check if they were identified correctly.
Species directories like "blank" or "team" can be ignored using \code{excludeSpecies}. If only specific stations are to be checked, \code{stationsToCheck} can be set. Values must match station directory names.
}
\value{
A \code{data.frame} listing all images  with their file names, directory and time stamp that were taken within a certain time of another species image at a particular station.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
 \note{
The function will not be able to find "isolated" images, i.e. images that were misidentified, but were not part of a sequence of images . Likewise, if all images of a sequence were misidentified, they cannot be found either.
In addition, it requires the following directory structure: Station/Species. As of version 0.98.0, it cannot check within the directory structure Station/Camera/Species or images identified by metadata tagging.
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

if (Sys.which("exiftool") != ""){        # only run this example if Exiftool is available
check.folders <- checkSpeciesFolders(inDir               = wd_images_ID,
                                     hasCameraSubfolders = FALSE,
                                     maxDeltaTime        = 120,
                                     writecsv            = FALSE)
                                     
check.folders   # In the example, 2 different species were photographed within 2 minutes. 

# now exclude one of these 2 species 
check.folders2 <- checkSpeciesFolders(inDir               = wd_images_ID,
                                      hasCameraSubfolders = FALSE,
                                      maxDeltaTime        = 120,
                                      excludeSpecies      = "EGY",
                                      writecsv            = FALSE)           
  
check.folders2  

# now we check only one station
check.folders3 <- checkSpeciesFolders(inDir               = wd_images_ID,
                                      hasCameraSubfolders = FALSE,
                                      maxDeltaTime        = 120,
                                      stationsToCheck     = "StationB",
                                      writecsv            = FALSE)


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
