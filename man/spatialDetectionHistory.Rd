\name{spatialDetectionHistory}
\alias{spatialDetectionHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a \code{capthist} object for spatial capture-recapture analyses from camera-trapping data
}
\description{
This function generates spatial detection histories of individuals of a species for spatial capture-recapture analyses with package \code{\link[secr]{secr}}. Data are stored in a \code{\link[secr]{capthist}} object. The \code{\link[secr]{capthist}} object contains detection histories, camera-trap station location and possibly individual and station-level covariates. Detection histories can have adjustable occasion length and occasion start time (other than midnight).
}
\usage{
spatialDetectionHistory(recordTable, 
  species, 
  camOp, 
  CTtable,
  stationCol = "Station", 
  speciesCol = "Species",
  Xcol,
  Ycol,
  stationCovariateCols,
  individualCol,
  individualCovariateCols,
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  occasionLength, 
  occasionStartTime = 0, 
  maxNumberDays,
  beginWithDay1,
  includeEffort,
  scaleEffort,
  binaryEffort,
  timeZone
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordDatabaseIndividual}} (or \code{\link{recordDatabase}})
}
  \item{species}{
  character. the species for which to compute the detection history
}
  \item{camOp}{
  The camera operability matrix as created by \code{\link{cameraOperation}} 
}
  \item{CTtable}{
  data.frame. contains station IDs and coordinates
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{recordTable} and \code{CTtable}
}
  \item{speciesCol}{
  character. name of the column specifying species in \code{recordTable}
}
  \item{Xcol}{
  character. name of the column specifying x coordinates in \code{CTtable}
}
  \item{Ycol}{
  character. name of the column specifying y coordinates in \code{CTtable}
}
  \item{stationCovariateCols}{
  character. name of the column(s) specifying station-level covariates in \code{CTtable}
}
  \item{individualCol}{
  character. name of the column specifying individual IDs in \code{recordTable}
}
  \item{individualCovariateCols}{
  character. name of the column(s) specifying individual covariates in \code{recordTable}
}
 \item{recordDateTimeCol}{
  character. name of the column specifying date and time in \code{recordTable}
}
  \item{recordDateTimeFormat}{
  format of column \code{recordDateTimeCol} in \code{recordTable}
}
  \item{occasionLength}{
  integer. occasion length in days
}
  \item{occasionStartTime}{
  integer. time of day (the full hour) at which to begin occasions.
}
  \item{maxNumberDays}{
  integer. maximum number of trap days per station (optional)
}
  \item{beginWithDay1}{
  logical. If TRUE, each station's detection history will begin at its respective setup day. If FALSE, occasions for all station will begin on the day the first station was set up.
}
  \item{includeEffort}{
  logical. Include trapping effort (number of active camera trap days per station and occasion) as usage in \code{\link[secr]{capthist}}  object?
}
  \item{scaleEffort}{
  logical. scale and center effort matrix to mean = 0 and sd = 1?
}
  \item{binaryEffort}{
  logical. Should effort be binary (1 if >1 active day per occasion, 0 otherwise)?
}
  \item{timeZone}{
  character. must be an argument of \code{\link[base]{OlsonNames}}
}

}
\details{
  The function creates a \code{\link[secr]{capthist}} object by combining three different objects: 1) a record table of identified individuals of a species, 2) a camera trap station table with trap coordinates and 3) a camera operation matrix computed with \code{\link{cameraOperation}}. The record table must contain a column with individual IDs and optionally individual covariates. The camera trap station table must contain station coordinates and optionally station-level covariates. The camera operation matrix provides the dates stations were active or not.
\code{beginWithDay1} defines if each stations detection history will begin on that station's setup day (\code{beginWithDay1 = TRUE}) or if all station's detection histories have a common origin (the day the first station was set up, \code{beginWithDay1 = TRUE}). 
\code{includeEffort} controls whether an effort matrix is computed or not. If TRUE, effort will be used for object \code{\link[secr]{usage}}) information in a \code{\link[secr]{traps}}). \code{scaleEffort} and \code{binaryEffort} further define the behaviour. 
The number of days that are aggregated is controlled by \code{occasionLength}, which must be between 1 and half the number of days in \code{camOp}.
\code{occasionStartTime} can be used to make occasions begin another hour than midnight (the default). This may be relevant for nocturnal animals, in which 1 whole night would be considered an occasion.     
}
\value{
A \code{\link[secr]{capthist}} object.
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

 \seealso{
\pkg{secr}
 }
\examples{

data(recordDatabaseIndividualSample)
data(camtraps)

# create camera operation matrix (with problems/malfunction)
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "\%d/\%m/\%Y"
)

sdh <- spatialDetectionHistory(recordTable          = recordDatabaseIndividualSample, 
                               species              = "LeopardCat",    
                               camOp                = camop_problem, 
                               CTtable              = camtraps,
                               stationCol           = "Station", 
                               speciesCol           = "Species",
                               Xcol                 = "utm_x",
                               Ycol                 = "utm_y",
                               individualCol        = "Individual",
                               recordDateTimeCol    = "DateTimeOriginal",
                               recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
                               occasionLength       = 10, 
                               beginWithDay1        = FALSE,
                               includeEffort        = TRUE,
                               timeZone             = "Asia/Kuala_Lumpur"
  )
  
# missing space in species = "LeopardCat" was introduced by recordDatabaseIndividual 
# (because of CRAN package policies. You can have spaces in your directory names)

  summary(sdh)
  plot(sdh, tracks = TRUE)
}