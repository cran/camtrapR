\name{checkSpeciesFolders}
\alias{checkSpeciesFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Consistency check on species image identification
}
\description{
This function assesses possible misidentification of species or mistakes in moving images to species directories. Within each station, it assesses whether there are images of a species taken within a given time interval in other species directories. Often, it is unlikely that different species are encountered within a very short time intervals at the same location.
This type of misidentification can arise easily if some images belonging to a sequence of images were moved into different species directories.
}
\usage{
checkSpeciesFolders(inDir, 
  maxDeltaTime, 
  exclude,
  writecsv = FALSE
)
}

\arguments{
  \item{inDir}{
  chraracter. Directory containing station subdirectories, which in turn contain renamed and identified images in species subdirectories (e.g. inDir/StationA/Species1)
}
  \item{maxDeltaTime}{
  numeric. Maximum time interval between images to be returned (in seconds)
}
  \item{exclude}{
  character. vector of species directories to exclude from check
}
  \item{writecsv}{
  logial. Should the resulting data.frame be saved as a .csv?
}
}
\details{
Images may accidentally be moved into wrong directories when identifying species by moving images into directories. Imagine your camera takes 3 shots every time it is triggered, and you move those three images into different species directories. The time difference will be very small (e.g. a few seconds). This function will return all these images for you to check if they were identified correctly.
Directories like "blank" or "team" can be ignored using \code{exclude}.
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
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

maxTimeDiff <- 120   # = 2 minutes

 if (Sys.which("exiftool") != ""){        # only run this example if Exiftool is available
check.folders <- checkSpeciesFolders(inDir = wd_images_ID,
  maxDeltaTime = maxTimeDiff,
  writecsv = FALSE)
                                     
check.folders   
# In this example, these are really 2 different species photographed within 2 minutes. 
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
