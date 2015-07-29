\name{getSpeciesImages}
\alias{getSpeciesImages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fetch all images of a species
}
\description{
This function will fetch all images of a particular species at all camera trap stations and copies these species' images to a new location. Images must be in the directory structure that is necessary for other functions of this package, too (species subdirectories within station directories). Images should be renamed prior to running the function to give them unique names based on station ID and date/time.
}
\usage{
getSpeciesImages(species, 
  inDir, 
  outDir, 
  stationSubfolders = FALSE, 
  emptyOutDir = FALSE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
character. species name for which the images are to be fetched
}
  \item{inDir}{
Directory in which to look for images. This is the directory containing the station directories, which in turn contain species directories. 
}
  \item{outDir}{
Directory in which to save species images (in a species subdirectory) 
}
  \item{stationSubfolders}{
logical. within the newly created species directory in \code{outDir}, create station directories for images?
}
  \item{emptyOutDir}{
logical. If TRUE, all .jpg images in the species folder in \code{outDir} will be deleted prior to copying. If FALSE, nothing will be deleted (default)
}
}
\details{
Rename the images in the folder structure using function \code{\link{imageRename}} to provide unique file names and prevent several images having the same name (if generic names like "DCIM0001.jpg" are used).
\code{inDir} must contain station directories, which in turn contain species image subdirectories of name \code{species}.
If \code{emptyOutDir} is TRUE, all .jpg images will be deleted from the target folder \code{outDir/species} recursively (in case there are station subdirectories). This may cause data loss, so please be careful. If \code{emptyOutDir} is FALSE and the target directory is not empty, an error will inform you and nothing will be copied, overwritten or deleted.
}
\value{
A data.frame with old and new file locations and the copy status (\code{copy_ok}; TRUE if copying was successful).
}

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
wd_images_ID_copy <- paste(getwd(), "sample_species_images", sep = "/")

species_to_copy <- "VTA"    # = Viverra tangalunga, Malay Civet

specImagecopy <- getSpeciesImages(species = species_to_copy,
  inDir = wd_images_ID,
  outDir = wd_images_ID_copy,
  stationSubfolders = FALSE,
  emptyOutDir = FALSE
  )
}
}