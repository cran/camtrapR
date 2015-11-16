\name{createStationFolders}
\alias{createStationFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create camera trap station directories, optionally with camera subdirectories
}
\description{
This function creates camera trap station directories, if needed with camera subdirectories. They can be used as an initial directory structure for storing raw camera trap images.
}
\usage{
createStationFolders(inDir, 
  stations, 
  cameras
)
}

\arguments{
  \item{inDir}{
  character. Directory in which directories are to be created
}
  \item{stations}{
  character. Station IDs to be used as directory names within \code{inDir}
}
  \item{cameras}{
  character. Camera trap IDs to be used as subdirectory names in each station directory (optionally)
}

}
\details{

The empty directories this function sets up serve as containers for saving raw camera trap images. If more than 1 camera was set up at a station, specifying \code{cameras} makes sense in order to keep images from different cameras separate.

}
\value{
  A \code{data.frame} with station (and possibly camera) directory names and an indicator for whether they were created successfully.
}
\author{
Juergen Niedballa
}

\examples{
\dontrun{

# create dummy directory for tests (this will be used as inDir)
# (normally, you'd set up an empty directory, e.g. .../raw_images)
wd_createStationDir <- paste(getwd(), "createStationFoldersTest", sep = "/")

# now we load the sample camera trap station data frame
data(camtraps)

# species names for which we want to create subdirectories
stations2create <- as.character(camtraps$Station)

# create station directories in 
StationFolderCreate1 <- createStationFolders (inDir    = wd_createStationDir,
                                              stations = stations2create)
  
StationFolderCreate1

# check if directories were created
list.dirs(wd_createStationDir)

}
}
