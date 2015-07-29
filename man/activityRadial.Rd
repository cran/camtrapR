\name{activityRadial}
\alias{activityRadial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Radial plots of single-species activity
}
\description{
The function generates a radial plot of species diel activity using an adapted version of function \code{\link[plotrix]{radial.plot}} from package \pkg{\link[plotrix]{plotrix}} (without the need to install the package). Records are aggregated by hour. The number of independent events is used as input, which in turn is based on the argument \code{minDeltaTime} in \code{\link{recordDatabase}}.
}
\usage{
  activityRadial(recordTable, 
  species,
  all.species = FALSE,
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  byNumber = FALSE,
  plotR = TRUE, 
  writePNG = FALSE, 
  plotDirectory, 
  createDir = FALSE, 
  pngMaxPix = 1000,
  ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordDatabase}}
}
  \item{species}{
  Name of the single species for which to create an kernel density plot of activity
}
  \item{all.species}{
  logical. If TRUE, plots are created for all species. This overrides argument \code{species}
}
 \item{speciesCol}{
  character. name of the column specifying species in \code{recordTable}
}
  \item{recordDateTimeCol}{
  character. name of the column specifying date and time in \code{recordTable}
}
  \item{recordDateTimeFormat}{
  format of column \code{recordDateTimeCol} in \code{recordTable}
}
  \item{byNumber}{
  logical. If FALSE, plot proportion of records. If TRUE, plot number of records
}
  \item{plotR}{
  logical. Create plots in R graphics device?
}
 \item{writePNG}{
  logical. Create pngs of the plots?
}
  \item{plotDirectory}{
  character. Path of the pngs
}
  \item{createDir}{
  logical. Create \code{plotDirectory}?
}
  \item{pngMaxPix}{
  integer. number of pixels in pngs along x axis
}
  \item{\dots}{
  additional arguments to be passed to function \code{\link[plotrix]{radial.plot}}
}
}
\details{

\code{radial.plot} was adjusted to show a clockwise 24-hour clock face. It is recommended to set argument \code{lwd} to a value >= 2. You may also wish to add argument \code{rp.type="p"} to show a polygon instead of bars.
}
%\note{
%These plots can be considered radial histograms.
%}
\value{
It returns invisibly a data.frame containing all information needed to create the plot: radial position, lengths, hour (for labels).
If \code{allSpecies == TRUE}, all species' data frames are returned in an invisible named list.
}
\references{
Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12. \cr
\url{http://CRAN.R-project.org/package=plotrix }
}

\author{
Juergen Niedballa
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{activityDensity}}, \code{\link{activityHistogram}}, \code{\link{activityOverlap}}
}
\examples{

# load record database
data(recordDatabaseSample)

species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet

activityRadial(recordTable = recordDatabaseSample,
  species = species4activity,
  all.species = FALSE,
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  plotR = TRUE,
  writePNG = FALSE,
  lwd = 5
)
}
