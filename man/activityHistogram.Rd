\name{activityHistogram}
\alias{activityHistogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot histogram of single-species activity
}
\description{
The function generates a histogram of species diel activity in 1-hour intervals.
}
\usage{
activityHistogram(recordTable, 
	species, 
	all.species = FALSE, 
	speciesCol = "Species", 
	recordDateTimeCol = "DateTimeOriginal", 
	recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S", 
	plotR = TRUE, 
	writePNG = FALSE, 
	plotDirectory, 
	createDir = FALSE, 
	pngMaxPix = 1000,
	...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
	data.frame. the record table created by \code{\link{recordDatabase}}
}
  \item{species}{
Name of the single species for which to create a histogram of activity
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
	additional arguments to be passed to function \code{hist}
}
}
\details{
Activity is calculated from the time of day of records. The date is ignored.
}
 \value{
  It returns invisibly a vector of species record date and time in \code{POSIXlt} format.
If \code{allSpecies == TRUE}, all species' vectors are returned in an invisible named list.
 }

\note{ 
If you have a sufficiently large number of records you may wish to consider using \code{\link{activityDensity}} instead. Please be aware that the function (like the other activity... function of this package) use clock time. If your survey was long enough to see changes in sunrise and sunset times, this may result in biased representations of species activity.
}

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{activityDensity}}, \code{\link{activityRadial}}, \code{\link{activityOverlap}}
}
\examples{

# load record database
data(recordDatabaseSample)

# generate activity histogram
species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet

activityHistogram (recordTable = recordDatabaseSample,
                   species = species4activity,
                   all.species = FALSE)
}
