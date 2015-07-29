\name{exifTagNames}
\alias{exifTagNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Exif metadata tag names from a jpg image
}
\description{
The function will return tag names of Exif metadata of jpg images. It uses the first jpg image from a subdirectory of the specified directory.
}
\usage{
exifTagNames(inDir, whichSubDir = 1)
}

\arguments{
  \item{inDir}{
  a character vector representing a directory containg camera trap images
}
\item{whichSubDir}{
  integer. The number of the subdirectory of \code{inDir} in which to look for an image
}
}
\details{
Many digital cameras record information such as ambient temperature or moon phase under ma\-nu\-fact\-ur\-er-specific tag names in Exif meta\-data of jpg images. If the tag names are known, they can passed to the \code{\link{recordDatabase}} function via the \code{metadataTags} argument to extract those information from images and add them to the record database.
}
\value{
A character vector containing available metadata tag names.
}
\references{
Phil Harvey's Exiftool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ } \cr
}
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{recordDatabase}}
}
\examples{
if (Sys.which("exiftool") != ""){        # only run this example if Exiftool is available
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
exifTagNames(inDir = wd_images_ID)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
