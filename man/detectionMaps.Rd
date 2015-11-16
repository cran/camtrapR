\name{detectionMaps}
\alias{detectionMaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate maps of species richness and species presence by station
}
\description{
Generates maps of species presence for all species that were recorded and the number of species by station. Output can be R graphics, PNG graphics or a shapefile for use in GIS software.
}
\usage{
detectionMaps(CTtable, 
  recordTable, 
  Xcol, 
  Ycol, 
  stationCol = "Station", 
  speciesCol = "Species", 
  richnessPlot = TRUE, 
  speciesPlots = TRUE,
  addLegend = TRUE,
  printLabels = FALSE, 
  smallPoints,
  plotR = TRUE, 
  writePNG = FALSE, 
  plotDirectory, 
  createPlotDir = FALSE,       
  pngMaxPix = 1000,
  writeShapefile = FALSE,
  shapefileName,
  shapefileDirectory,
  shapefileProjection
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CTtable}{
  data.frame. contains station IDs and coordinates
}
  \item{Xcol}{
  character. name of the column specifying x coordinates in \code{CTtable}
}
  \item{Ycol}{
  character. name of the column specifying y coordinates in \code{CTtable}
}
  \item{stationCol}{
  character. name of the column specifying station ID in \code{CTtable} and \code{recordTable}
}
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordDatabase}}
}
  \item{speciesCol}{
  character. name of the column specifying species in \code{recordTable}
}
  \item{writePNG}{
  logical. Create PNGs of the plots?
}
  \item{plotR}{
  logical. Create plots in R graphics device?
}
  \item{plotDirectory}{
  character. Directory in which to save the PNGs
}
  \item{createPlotDir}{
  logical. Create \code{plotDirectory}?
}
  \item{richnessPlot}{
  logical. Generate a species richness plot? 
}
  \item{speciesPlots}{
  logical. Generate plots of all species number of independent events?
}
  \item{printLabels}{
  logical. Add station labels to the plots?
}
  \item{smallPoints}{
  numeric. Number by which to decrease point sizes in plots (optional).
}
  \item{addLegend}{
  logical. Add legends to the plots?
}
  \item{pngMaxPix}{
  integer. number of pixels in pngs on the longer side
}
  \item{writeShapefile}{
  logical. Create a shapefile from the output?
}
  \item{shapefileName}{
  character. Name of the shapefile to be saved. If empty, a name will be generated automatically.
}
  \item{shapefileDirectory}{
  character. Directory in which to save the shapefile.
}
  \item{shapefileProjection}{
  character. A character string of projection arguments to use in the shapefile.
}
}
 \details{
The column name \code{stationCol} must be identical in \code{CTtable} and \code{recordTable}.
Shapefile creation depends on the packages \pkg{sp} and \pkg{rgdal}. \code{shapefileProjection} must be a valid argument of \code{\link[sp]{CRS}}. If \code{shapefileProjection} is undefined, the resulting shapefile will lack a coordinate reference system.
 }
\value{
A \code{data.frame} with station coordinates, numbers of events by species at each station and total species number by station. In addition and optionally, R graphics or png image files.
}

\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{

# load station information
data(camtraps)

# load record database
data(recordDatabaseSample)

# create maps
Mapstest <- detectionMaps(CTtable      = camtraps,
                          recordTable  = recordDatabaseSample,
                          Xcol         = "utm_x",
                          Ycol         = "utm_y",
                          stationCol   = "Station",
                          speciesCol   = "Species",
                          writePNG     = FALSE,
                          plotR        = TRUE,
                          printLabels  = TRUE,
                          richnessPlot = TRUE,
                          addLegend    = TRUE
)
}

\references{
A great resource for \code{\link[sp]{CRS}} arguments is \url{http://spatialreference.org/}. Use the Proj4 string as \code{shapefileProjection} argument.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
