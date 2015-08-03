\name{appendSpeciesNames}
\alias{appendSpeciesNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add or remove species names from image filenames
}
\description{
Function to add or remove species names from jpg image filenames. Images must be sorted into station directories and species subdirectories.
}
\usage{
appendSpeciesNames(inDir, 
  removeNames = FALSE, 
  writecsv = FALSE
)
}

\arguments{

  \item{inDir}{
  Directory containing station directories with renamed and identified images in species directories (e.g. inDir/StationA/Species1)
}
  \item{removeNames}{
 logical. remove appended species names?
}
  \item{writecsv}{
 logical. write csv table containing old and new file names into \code{inDir}?
}
}

\details{
 Species names can be appended or removed. If species names were appended several times by accident, they can all be removed by running the function with \code{removeNames = TRUE}. Before running the function, you may want to run \code{\link{checkSpeciesFolders}} to detect possible misidentifications. As an example, the function would change an image file name from "StationA__2015-05-41__20-59-59(1).JPG" to "StationA__2015-05-41__20-59-59(1)__Species Name.JPG".
}
\value{
A \code{data.frame} containing the old and new file names and directories.
}
% \references{
% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{

# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = getwd(), recursive = TRUE)
wd_images_ID_copy <- paste(getwd(), "sample_images", sep = "/")

# append species names
SpecNameAppend1 <- appendSpeciesNames(inDir = wd_images_ID_copy,
  removeNames = FALSE,
  writecsv = FALSE)
  
SpecNameAppend1

# remove species names
SpecNameRemove1 <- appendSpeciesNames(inDir = wd_images_ID_copy,
  removeNames = TRUE,
  writecsv = FALSE)
  
SpecNameRemove1
}
}
