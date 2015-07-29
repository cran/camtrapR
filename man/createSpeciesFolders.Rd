\name{createSpeciesFolders}
\alias{createSpeciesFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create species folders for species identification
}
\description{
This function creates species subdirectories within station directories. They can be used for species identification by manually moving images into the respective species directories. It can also delete empty species directories (if species were not detected at sites).
}
\usage{
createSpeciesFolders(inDir, 
  species, 
  removeFolders = FALSE
)
}

\arguments{
  \item{inDir}{
  character. Directory containing station subdirectories with renamed images (output of  \code{\link{imageRename}})
}
  \item{species}{
  character. names of species directories to be created in every (station) subdirectory of \code{inDir}
}
  \item{removeFolders}{
  logical. Indicating whether to create (TRUE) or remove species directories (FALSE).
}

}
\details{

Empty directories can be created as containers for species identification, which should be done via drag and drop (i.e. moving images into species directories, not copying). After this, empty species directories can be deleted using \code{removeFolders = TRUE}. 
}
\value{
  A data.frame with directory names and an indicator for whether they exist.
}
\author{
Juergen Niedballa
}

\examples{
\dontrun{

# create dummy directories for tests
# (normally, you'd use directory containing renamed, unsorted images)

# this will be used as inDir
wd_createDirTest <- paste(getwd(), "createSpeciesFoldersTest", sep = "/")

# now we create 2 station subdirectories
dirs_to_create <- paste(wd_createDirTest, c("StationA", "StationB"), sep = "/")
sapply(dirs_to_create, FUN = dir.create, recursive = TRUE)

# species names for which we want to create subdirectories
species <- c("Sambar Deer", "Bay Cat")

# create species subdirectories
SpecFolderCreate1 <- createSpeciesFolders (inDir = wd_createDirTest,
  species = species,
  removeFolders = FALSE)
  
SpecFolderCreate1

# check if directories were created
list.dirs(wd_createDirTest)

# delete empty species directories
SpecFolderCreate2 <- createSpeciesFolders (inDir = wd_createDirTest,
  species = species,
  removeFolders = TRUE)

SpecFolderCreate2

# check if directories were deleted
list.dirs(wd_createDirTest)

}
}
