\name{detectionHistory}
\alias{detectionHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species detection histories for occupancy analyses
}
\description{
This function generates species detection histories that can be used in occupancy analyses, e.g. with package \code{\link[unmarked]{unmarked}}. It generates detection histories in different formats, with adjustable session length and session start time (other than midnight).
}
\usage{
detectionHistory(recordTable, 
  species, 
  camOp, 
  stationCol = "Station", 
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  sessionLength, 
  maxNumberDays,
  BeginWithDay1 = FALSE,
  includeEffort = TRUE,
  minimumEffort,
  scaleEffort,
  SessionStartTime = 0, 
  DatesAsSessionNames = FALSE,
  timeZone,
  writecsv = FALSE, 
  outDir
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordDatabase}}
}
  \item{species}{
  character. the species for which to compute the detection history
}
  \item{camOp}{
  The camera operability matrix as created by \code{\link{cameraOperation}} 
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{recordTable}
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
  \item{sessionLength}{
  integer. session length in days
}
  \item{maxNumberDays}{
  integer. maximum number of trap days per session (optional)
}
  \item{BeginWithDay1}{
  logical. If TRUE, each station's detection history will begin at its respective setup day. If FALSE, sessions for all station will begin on the day the first station was set up.
}
  \item{includeEffort}{
  logical. Compute effort (active camera trap days per station and session)?
}
   \item{minimumEffort}{
  integer. minimum number of active trap days (effort) for a station to be considered active
}
  \item{scaleEffort}{
  logical. scale and center effort matrix to mean = 0 and sd = 1?
}
  \item{SessionStartTime}{
  integer. time of day (the full hour) at which to begin sessions.
}
  \item{DatesAsSessionNames}{
  If \code{BeginWithDay1} is \code{FALSE}, session names in the detecion history will be composed of first and last day of that session. 
}
  \item{timeZone}{
  character. must be an argument of \code{\link[base]{OlsonNames}}
}
  \item{writecsv}{
  logical. Should the detection history be saved as a .csv?
}
  \item{outDir}{
  character. Directory into which detection history .csv file is saved
}

}
\details{
  The function computes a species detection matrix, either as a detection-by-date or a detection-by-session matrix. The behaviour is controlled by the argument \code{BeginWithDay1}. If \code{BeginWithDay1} is TRUE, each stations history will begin on that station's setup day. If \code{BeginWithDay1} is FALSE, all station's detection histories have a common origin (the day the first station was set up). 
  
  \code{includeEffort} controls whether an effort matrix is computed or not. This also affects the detection matrices. If FALSE, all sessions in which a station was not set up or malfunctioning (NA or 0 in \code{camOp}) will result in NAs in the detection history. If TRUE, the record history will only contain 0 and 1, and no NAs. The effort matrix can then be included in occupancy models as a (continuous) observation covariate to estimate the effect of effort on detection probability.
  The number of days that are aggregated is controlled by \code{sessionLength}, which must be between 1 and half the number of days in \code{camOp}.
  \code{SessionStartTime} can be used to make sessions begin another hour than midnight (the default). This may be relevant for nocturnal animals, in which 1 whole night would be considered an occasion.   
  The values of \code{stationCol} in \code{recordTable} must be matched by the row names of \code{camOp} (case-insensitive), otherwise an error is raised.
  \code{DateTimeFormat} defaults to "\%Y-\%m-\%d \%H:\%M:\%S", e.g. "2014-09-30 22:59:59". For details on how to specify date and time formats in R see \code{\link[base]{strptime}}.
}
\value{
Depending on the value of \code{includeEffort} and \code{scaleEffort}, a list with either 1, 2 or 3 elements. The first element is the species detection history, the second the optional effort matrix. 
  \item{detectionHistory}{A species detection matrix}
  \item{Effort}{A matrix giving the number of active camera trap days per station and session (= camera trapping effort). It is only returned if \code{includeEffort} is \code{TRUE}}
%% ...
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

% \seealso{
% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{

# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# load station information
data(camtraps)

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable = camtraps,
  stationCol = "Station",
  setupCol = "Setup_date",
  retrievalCol = "Retrieval_date",
  hasProblems = FALSE,
  dateFormat = "\%d/\%m/\%Y"
)

# create record database
minDeltaT <- 60   # minutes between independent observations

if (Sys.which("exiftool") != ""){        # only run this function if Exiftool is available
recordDatabaseSample <- recordDatabase(inDir = wd_images_ID,
  minDeltaTime = minDeltaT,
  exclude = "NO_ID"
)
} else {
data(recordDatabaseSample)
}

# compute detection history for a species
species_for_DetHist <- "VTA"    # = Viverra tangalunga, Malay Civet
days_per_session <- 7

DetHist1 <- detectionHistory(recordTable = recordDatabaseSample,
  camOp = camop_no_problem,
  stationCol = "Station",
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  species = species_for_DetHist,
  sessionLength = days_per_session,
  BeginWithDay1 = TRUE,
  DatesAsSessionNames = FALSE,
  includeEffort = FALSE
)

DetHist1
}
