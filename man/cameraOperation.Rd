\name{cameraOperation}
\alias{cameraOperation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a camera operability matrix
}
\description{
Construct a camera operability matrix for use in \code{\link{detectionHistory}}. If there were several cameras per station, they can be aggregated using different rules.
}
\usage{
  cameraOperation(CTtable, 
  stationCol, 
  cameraCol, 
  setupCol, 
  retrievalCol, 
  hasProblems = FALSE,
  byCamera, 
  allCamsOn, 
  sumUpCameras,
  dateFormat = "\%Y-\%m-\%d", 
  writecsv = FALSE, 
  outDir
)
}

\arguments{
  \item{CTtable}{
    data.frame containing information about location and trapping period of camera trap stations
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{CTtable}
}
  \item{cameraCol}{
  character. name of the column specifying Camera ID in \code{CTtable} (optional)
}
  \item{setupCol}{
  character. name of the column containing camera setup dates in \code{CTtable}
}
  \item{retrievalCol}{
  character. name of the column containing camera retrieval dates in \code{CTtable}
}
  \item{hasProblems}{
  logical. If TRUE, function will look for columns specifying malfunction periods in \code{CTtable} (naming convention: \code{ProblemX_from} and \code{ProblemX_to}, where X is a number)
}
  \item{byCamera}{
  logical. If TRUE, camera operability matrix is computed by camera, not by station (requires \code{cameraCol})
}
  \item{allCamsOn}{
  logical. If TRUE, all cameras of a station need to be operational for a station to be operational (only relevant if \code{byCamera} is FALSE). Output values will be 1/0/NA only (all cameras at a station operational/ at least 1 camera not operational/no camera set up)
}
  \item{sumUpCameras}{
  logical. Sum up number of active camera traps by station? Only if \code{byCamera} is FALSE and \code{allCamsOn} is FALSE. If \code{sumUpCameras} is TRUE, output values will be n/NA. n is the number of operational cameras at a station. If \code{sumUpCameras} is FALSE, n is 1 if at least 1 camera was operational, otherwise 0. In both cases, values are NA if no camera was set up.
  }
  \item{dateFormat}{
  character. The format of columns \code{setupCol} and \code{retrievalCol}. Should be interpretable by \code{as.Date}
}
  \item{writecsv}{
  logical. Should the camera operability matrix be saved as a .csv?
}
  \item{outDir}{
  character. Directory into which csv is saved
}
}
\details{
\code{cameraCol} is NULL by default. The function will check if there is more than 1 entry for any station and return an error if TRUE. In that case \code{cameraCol} needs to be specified to identify individual cameras within a station.
\code{dateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31". See \code{\link[base]{strptime}} for formatting options.
If \code{hasProblems} is TRUE, the function tries to find columns \code{ProblemX_from} and \code{ProblemX_to} in \code{CTtable}. \code{X} designates numbers from 1 to n in which a camera or station was not operational. If FALSE, cameras are assumed to have been operational from setup to retrieval.
\code{allCamsOn} only has an effect if there is more than 1 camera at a station. If it is TRUE, all cameras need to be operational for the station to be considered operational. If FALSE, at least 1 active station renders the station operational.
Likewise, \code{sumUpCameras} only has an effect if there is more than 1 camera at a station. If TRUE, 2 active cameras will return the value 2. If FALSE, 2 active cameras will return 1. The behaviour with malfunctioning cameras depends on \code{allCamsOn}. 


}
\value{
  A matrix. Row names indicate Station ID, column names are dates. \cr
Legend: NA: camera(s) not set up, 0: camera(s) not operational, 1 (or higher): number of operational camera(s)/camera(s) were operational (depending on \code{sumUpCameras} and \code{allCamsOn})  
}
% \references{
% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
 \note{
Setting \code{sumUpCameras} according to the sampling situation is important for function \code{\link{detectionHistory}}, if sampling effort (the number of active trap nights in a session) is to be computed and included as a detection covariate in models. If 2 cameras face each other, the second one does not increase detection probability compared to 1 camera only (if both are triggered reliably by species). In contrast, if cameras at a station do not face each other, the user may choose to consider detections at the cameras independent of one another. The user may then set \code{sumUpCameras} in this function to TRUE and set \code{includeEffort} in \code{\link{detectionHistory}} to TRUE and use the resulting effort matrix as an observation covariate in occupancy modelling.
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{

data(camtraps)

# no problems/malfunction
camop_no_problem <- cameraOperation(CTtable = camtraps,
  stationCol = "Station",
  setupCol = "Setup_date",
  retrievalCol = "Retrieval_date",
  writecsv = FALSE,
  hasProblems = FALSE,
  dateFormat = "\%d/\%m/\%Y"
)

# with problems/malfunction
camop_problem <- cameraOperation(CTtable = camtraps,
  stationCol = "Station",
  setupCol = "Setup_date",
  retrievalCol = "Retrieval_date",
  writecsv = FALSE,
  hasProblems = TRUE,
  dateFormat = "\%d/\%m/\%Y"
)

camop_no_problem
camop_problem
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
