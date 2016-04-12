\name{getSpeciesImages}
\alias{getSpeciesImages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collect all images of a species
}
\description{
This function will fetch all images of a particular species from all camera trap stations and copies these images to a new location. Species IDs can be read from species directories or from metadata. Earlier in the workflow, images should have been renamed (with \code{\link{imageRename}}) to give images unique file names based on station ID and date/time.
}
\usage{
getSpeciesImages(species, 
  inDir, 
  outDir, 
  createStationSubfolders = FALSE,
  IDfrom,
  metadataSpeciesTag,
  metadataHierarchyDelimitor = "|"
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
character. Species whose images are to be fetched
}
  \item{inDir}{
character. Directory containing identified (species level) camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
  \item{outDir}{
character. Directory in which to save species images (in a species subdirectory) 
}
  \item{createStationSubfolders}{
logical. Save images in station directories within the newly created species directory in \code{outDir}?
}
\item{IDfrom}{
character. Read species ID from image metadata ("metadata") of from species directory names ("directory")?
}
\item{metadataSpeciesTag}{
  character. The species ID tag name in image metadata (if IDfrom = "metadata").
}
\item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":" (if IDfrom = "metadata").
}

}
\details{

The function can derive species IDs both from a directory structure like this
> inDir/Station/Species   or
> inDir/Station/Camera/Species, 

or from species metadata tags. In the latter case, only station directories are needed.
In any case, the argument \code{species} must match species IDs (either directory names or species metadata tags).

Before running the function, first rename the images using function \code{\link{imageRename}} to provide unique file names and prevent several images from having the same name (if generic names like "IMGP0001.jpg" are used). The function will not copy images if there are duplicate filenames to prevent overwriting images unintentionally.
}
\value{
A \code{data.frame} with old and new file locations and the copy status (\code{copy_ok}; TRUE if copying was successful).
}

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
wd_images_ID_copy <- file.path(tempdir(), "sample_species_images")

species_to_copy <- "VTA"    # = Viverra tangalunga, Malay Civet

specImagecopy <- getSpeciesImages(species                 = species_to_copy,
                                  inDir                   = wd_images_ID,
                                  outDir                  = wd_images_ID_copy,
                                  createStationSubfolders = FALSE,
                                  IDfrom                  = "directory"
                                  )
}
}