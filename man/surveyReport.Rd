\name{surveyReport}
\alias{surveyReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a report about camera trap surveys and species captures
}
\description{
This function creates a report about camera trap surveys and species captures from a camera information table and a record database (generated with \code{\link{recordDatabase}}). The function calls \code{\link{cameraOperation}} internally.
}
\usage{
surveyReport (recordTable,
  CTtable,
  speciesCol = "Species",
  stationCol = "Station",
  cameraCol, 
  setupCol,
  retrievalCol,
  CTDateFormat = "\%Y-\%m-\%d", 
  CTHasProblems = FALSE,
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  sinkpath
)
}

\arguments{
  \item{recordTable}{
   data.frame containing a species record database as calculated by \code{\link{recordDatabase}}
}
  \item{CTtable}{
   data.frame containing information about location and trapping period of camera trap stations
}
  \item{speciesCol}{
  character. name of the column specifying Species ID in \code{CTtable}
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{CTtable}
}
  \item{cameraCol}{
  character. name of the column specifying Camera ID in \code{CTtable} 
}
  \item{setupCol}{
  character. name of the column containing camera setup dates in \code{CTtable}
}
  \item{retrievalCol}{
  character. name of the column containing camera retrieval dates in \code{CTtable}
}
  \item{CTDateFormat}{
  character. The format of columns \code{setupCol} and \code{retrievalCol} in \code{CTtable}. 
}
\item{CTHasProblems}{
  logical. Are there periods of camera malfunction specified in \code{CTtable}? This is passed as argument \code{hasProblems} to the function \code{\link{cameraOperation}}, which is called internally.
}
  \item{recordDateTimeCol}{
  character. The names of the column containing date and time of record in \code{recordTable}
}
  \item{recordDateTimeFormat}{
  character. The date/time format of column \code{recordDateTimeCol} in \code{recordTable}. 
}
  \item{sinkpath}{
character. The directory into which the survey report is saved (optional)
}
}
\details{


The value of \code{CTDateFormat} should be an interpretable by \code{\link[base]{as.Date}}
If \code{sinkpath} is defined, a txt file with the survey information will be saved there. If not, results will be shown in the console. 

The function will call \code{\link{cameraOperation}} internally to compute survey information from the camera operability database.
\code{cameraCol} is "Camera" by default. \code{\link{cameraOperation}} (called internally) will check if there is more than 1 entry for any station and return an error if TRUE and the camera ID column of \code{CTtable} is not "Camera". In that case \code{cameraCol} needs to be changed accordingly. \code{CTDateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31". See \code{\link[base]{strptime}} for how to format date and time strings in R.
In the \code{\link{cameraOperation}} call the argument \code{hasProblems} is set to TRUE, i.e. the function tries to find columns \code{ProblemX_from} and \code{ProblemX_to} in \code{CTtable}. \code{X} designates numbers from 1 to n in which a camera or station was not operational. If there are no such columns all stations are assumed to have been operational from setup to retrieval. 
}
\value{
 A list containing 5 \code{data.frames}. 

  \item{survey_dates}{station and image date ranges, number of trap days}
  \item{species_by_station}{species number by station}
  \item{events_by_species}{number of events and stations by species}
  \item{events_by_station}{number of events for every species by station (only species that were recorded)}
  \item{events_by_station2}{number of events for all species at all stations (including species that were not recorded)}
  
    
The output will be saved to a .txt file if \code{sinkpath} is defined.
}

\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cameraOperation}}
\code{\link{recordDatabase}}
}
\examples{
data(camtraps)
data(recordDatabaseSample)

reportTest <- surveyReport (recordTable = recordDatabaseSample,
              CTtable = camtraps,
              speciesCol = "Species",
              stationCol = "Station",
              setupCol = "Setup_date",
              retrievalCol = "Retrieval_date",
              CTDateFormat = "\%d/\%m/\%Y", 
              recordDateTimeCol = "DateTimeOriginal",
              recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S")

class(reportTest) # a list with
length(reportTest) # 5 elements

reportTest[[1]]    # camera trap operation times and image date ranges
reportTest[[2]]    # number of species by station
reportTest[[3]]    # number of events and number of stations by species
reportTest[[4]]    # number of species events by station
reportTest[[5]]    # number of species events by station including 0s (non-observed species)


\dontrun{
# run again with sinkpath defined
reportTest <- surveyReport (recordTable = recordDatabaseSample,
                            CTtable = camtraps,
                            speciesCol = "Species",
                            stationCol = "Station",
                            setupCol = "Setup_date",
                            retrievalCol = "Retrieval_date",
                            CTDateFormat = "\%d/\%m/\%Y", 
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
                            sinkpath = getwd())

# have a look at the text file
readLines(list.files(getwd(), pattern = ".txt$", full.names = TRUE))
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
