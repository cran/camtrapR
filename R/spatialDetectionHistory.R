spatialDetectionHistory <- function(recordTableIndividual,
                                    species,
                                    camOp,
                                    CTtable,
                                    output,
                                    stationCol = "Station",
                                    speciesCol = "Species",
                                    Xcol,
                                    Ycol,
                                    stationCovariateCols,
                                    individualCol,
                                    individualCovariateCols,
                                    recordDateTimeCol = "DateTimeOriginal",
                                    recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                    occasionLength,
                                    occasionStartTime = 0,
                                    maxNumberDays,
                                    day1,
                                    buffer,
                                    includeEffort = TRUE,
                                    scaleEffort = FALSE,
                                    binaryEffort = FALSE,
                                    timeZone,
                                    makeRMarkInput
)
{

  wd0 <- getwd()
  on.exit(setwd(wd0))

  #################
  # check input
  stopifnot(hasArg(recordTableIndividual))
  stopifnot(hasArg(camOp))

  stopifnot(hasArg(species))
  stopifnot(is.character(species))
  stopifnot(length(species) == 1)

  if(output %in% c("binary", "count") == FALSE) stop("'output' can only be 'binary' or 'count'")

  stopifnot(hasArg(occasionLength))

  stopifnot(c(speciesCol, recordDateTimeCol, stationCol, individualCol) %in% colnames(recordTableIndividual))


  stopifnot(hasArg(stationCol))
  stopifnot(stationCol %in% colnames(recordTableIndividual))
  stopifnot(length(stationCol) == 1)
  recordTableIndividual[,stationCol] <- as.character(recordTableIndividual[,stationCol])
  stopifnot(is.character(stationCol))

  stopifnot(hasArg(speciesCol))
  stopifnot(speciesCol %in% colnames(recordTableIndividual))
  stopifnot(length(speciesCol) == 1)
  recordTableIndividual[,speciesCol] <- as.character(recordTableIndividual[,speciesCol])
  stopifnot(is.character(speciesCol))

  stopifnot(hasArg(individualCol))
  stopifnot(individualCol %in% colnames(recordTableIndividual))
  stopifnot(length(individualCol) == 1)
  recordTableIndividual[,individualCol] <- as.character(recordTableIndividual[,individualCol])
  stopifnot(is.character(individualCol))

  stopifnot(hasArg(recordDateTimeCol))
  stopifnot(recordDateTimeCol %in% colnames(recordTableIndividual))
  stopifnot(length(recordDateTimeCol) == 1)
  recordTableIndividual[,recordDateTimeCol] <- as.character(recordTableIndividual[,recordDateTimeCol])
  stopifnot(is.character(recordDateTimeCol))

  if(hasArg(makeRMarkInput)) stopifnot(is.logical(makeRMarkInput))

  stopifnot(c(stationCol, Xcol, Ycol) %in% colnames(CTtable))
  if(any(is.na(CTtable[,Xcol])))stop("there are NAs in Xcol")
  if(any(is.na(CTtable[,Ycol])))stop("there are NAs in Ycol")
  if(any(is.na(CTtable[,stationCol])))stop("there are NAs in stationCol of CTtable")

  if(hasArg(stationCovariateCols)){
    stopifnot(stationCovariateCols %in% colnames(CTtable))
  }

  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }


  occasionStartTime    <- as.integer(round(occasionStartTime))
  if(occasionStartTime != 0 & !is.integer(occasionStartTime)) stop ("occasionStartTime must be between 0 and 23")
  if(occasionStartTime < 0 | occasionStartTime >= 24)         stop ("occasionStartTime must be between 0 and 23")

  occasionLength    <- as.integer(round(occasionLength))
  if(occasionLength <= 0)             stop("occasionLength must be a positive integer and not 0")
  if(occasionLength > ncol(camOp)/2)  stop("occasionLength may not be greater than half the total number of days in camOp")
  stopifnot(is.numeric(occasionLength))

  if(hasArg(maxNumberDays)){
    maxNumberDays    <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp))    stop("maxNumberDays must be smaller than the number of columns of camOp")
    if(maxNumberDays < occasionLength) stop("maxNumberDays must be larger than or equal to occasionLength")
  }

  if(hasArg(buffer)) {
    stopifnot(is.numeric(buffer))
    buffer <- round(buffer)
    stopifnot(buffer >= 1)
  }


  if(species %in% recordTableIndividual[,speciesCol] == FALSE) stop("species is not in speciesCol of recordTableIndividual")

  # check all stations in recordTableIndividual are matched in CTtable
  if(all(recordTableIndividual[,stationCol] %in% CTtable[,stationCol]) == FALSE) {
    stop(paste("items of stationCol in recordTableIndividual are not matched in stationCol of CTtable: ", paste(recordTableIndividual[-which(recordTableIndividual[,stationCol] %in% CTtable[,stationCol]),stationCol], collapse = ", ")))
  }


  if(includeEffort == TRUE){
    if(hasArg(scaleEffort)){
      if(class(scaleEffort) != "logical") stop("scaleEffort must be logical (TRUE or FALSE)")
    } 
    if(hasArg(binaryEffort)){
      if(class(binaryEffort) != "logical") stop("binaryEffort must be logical (TRUE or FALSE)")
    } 
    if(binaryEffort == TRUE & scaleEffort == TRUE) stop("'scaleEffort' and 'binaryEffort' cannot both be TRUE")
  } else {
  scaleEffort  <- FALSE
  binaryEffort <- FALSE
  }


  #####################################################################################################################
  # bring date, time, station ids into shape

  subset_species           <- subset(recordTableIndividual, recordTableIndividual[,speciesCol] == species)
  subset_species$DateTime2 <- as.POSIXlt(subset_species[,recordDateTimeCol], tz = timeZone, format = recordDateTimeFormat)

  # check consistency of argument day1
  stopifnot(class(day1) == "character")
  
  if(day1 == "survey") {day1switch <- 1} else {
    if(day1 == "station") {day1switch <- 2} else {
      try(date.test <- as.Date(day1), silent = TRUE)
       if(class(date.test) != "Date") stop('could not interpret argument day1: can only be "station", "survey" or a specific date (e.g. "2015-12-31")')
       if(hasArg(buffer)) stop("if buffer is defined, day1 can only be 'survey' or 'station'")
      suppressWarnings(rm(date.test))
      day1switch <- 3
    }
  }

  if("POSIXlt" %in% class(subset_species$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTableIndividual using specified recordDateTimeFormat")
  if(any(is.na(subset_species$DateTime2))) stop("at least 1 entry in recordDateTimeCol of recordTableIndividual could not be interpreted using recordDateTimeFormat")

  ####
  cam.op.worked0 <- as.matrix(camOp)

  if(all(as.character(unique(subset_species[,stationCol])) %in% rownames(cam.op.worked0)) == FALSE){
    (stop("Not all values of stationCol in recordTableIndividual are matched by rownames of camOp"))
  }



  ################################################
  # compute date range of stations and records

  arg.list0 <- list(cam.op = cam.op.worked0, subset_species_tmp = subset_species, stationCol_tmp = stationCol, day1_tmp = day1, occasionStartTime_tmp = occasionStartTime, timeZone_tmp = timeZone)

    if(hasArg(maxNumberDays))  arg.list0 <- c(arg.list0,   maxNumberDays_tmp = maxNumberDays)
    if(hasArg(buffer))   arg.list0 <- c(arg.list0, buffer_tmp =  buffer)

  date_ranges <- do.call(createDateRangeTable, arg.list0)

  rm(arg.list0)

  #######################
  # adjust camera operation matrix

  cam.op.worked <- adjustCameraOperationMatrix(cam.op = cam.op.worked0, date_ranges2 = date_ranges, timeZone_tmp = timeZone, day1_2 = day1)

  # append occasionStartTime (if != 0) to column names for output table
  if(occasionStartTime != 0){
    colnames(cam.op.worked) <- paste(colnames(cam.op.worked), "+", occasionStartTime, "h", sep = "")
  }

  ######################
  # calculate trapping effort by station and occasion
  effort.tmp <-  calculateTrappingEffort (cam.op = cam.op.worked, occasionLength2 = occasionLength, scaleEffort2 = scaleEffort, includeEffort2 = includeEffort)

  effort <- effort.tmp[[1]]
  if(isTRUE(scaleEffort))     scale.eff.tmp.attr <- effort.tmp[[2]]
  if(isTRUE(binaryEffort))    effort             <- ifelse(effort >= 1, 1, 0)


  ###################
  # remove records that fall into buffer period or were taken after maxNumberDays

  subset_species <- cleanSubsetSpecies(subset_species2 = subset_species, stationCol2 = stationCol, date_ranges2 = date_ranges)

  ############
  #  define the 1st day of the effective survey period.

if(day1 %in% c("survey")){
  time2 <- date_ranges$start_first_occasion_survey[match(subset_species[,stationCol], rownames(date_ranges))]
} else {
  time2 <- date_ranges$start_first_occasion[match(subset_species[,stationCol], rownames(date_ranges))]
}


#  time2 <- date_ranges$start_first_occasion[match(subset_species[,stationCol], rownames(date_ranges))]

  # calculate time difference between records and first day of detection history (the occasion each record belongs into)
  occasionCol <- "Occasion"

  subset_species[,occasionCol] <- as.numeric(ceiling((difftime(time1  = subset_species$DateTime2,
                                                          time2 =  time2,
                                                          units = "secs",
                                                          tz = timeZone)
                                                / (occasionLength * 86400))))


  if(max(subset_species[,occasionCol]) > ncol(effort)) {stop("encountered a bug. I'm Sorry. Please report it.")}


  # column names for effort matrix
  if(includeEffort == TRUE) {colnames(effort) <-  paste("o", seq(1,ncol(effort), by = 1), sep = "")}

  #############
  # build spatial detection history

  # get relevant columns from subset_species
  if(hasArg(individualCovariateCols)){
    sdh0 <- subset_species[, c(individualCol, occasionCol, stationCol, individualCovariateCols)]
    colnames(sdh0) <- c("ID", occasionCol, "trapID", individualCovariateCols)
  } else {
    sdh0 <- subset_species[, c(individualCol, occasionCol, stationCol)]
    colnames(sdh0) <- c("ID", occasionCol, "trapID")
  }

  # remove unidentified individuals
  remove.tmp <- which(is.na(sdh0[,"ID"]))
  if(length(remove.tmp) >= 1){
    sdh0 <- sdh0[-remove.tmp,]
    warning(paste("removed", length(remove.tmp), "records because of missing individual IDs"))
  }

# add required session column
  sdh1 <- data.frame(Session = 1, sdh0)   

# if requested,  remove duplicate records (makes capthist binary. otherwise it returns counts)
  if(output == "binary"){
    sdh2 <- unique(sdh1)                  # remove duplicate rows
  } else {
    sdh2 <- sdh1                          # keep duplicate rows 
  }
  

  ############
  # make secr traps object

  coord.ct <- CTtable[,c(Xcol, Ycol)]
  colnames(coord.ct) <- c("x", "y")
  rownames(coord.ct) <- CTtable[,stationCol]

# set detector type according to desired output (count or binary)
  detectortype <- ifelse (output == "binary", "proximity", "count")

  if(includeEffort == TRUE) {
    if(binaryEffort == TRUE){
      secr.traps <- read.traps(data         = coord.ct,
                               detector     = detectortype,
                               binary.usage = TRUE)
    } else {
      secr.traps <- read.traps(data         = coord.ct,
                               detector     = detectortype,
                               binary.usage = FALSE)
    }
    secr::usage(secr.traps) <- effort
  } else {
    secr.traps <- read.traps(data     = coord.ct,
                             detector = detectortype)
  }

  if(hasArg(stationCovariateCols)){
    whichstationCovariateCols <- which(colnames(CTtable) %in% stationCovariateCols)
    stationCovsDF <- data.frame(CTtable[match(rownames(secr.traps), CTtable[,stationCol]), whichstationCovariateCols])
    if(length(stationCovariateCols) == 1) {
      colnames(stationCovsDF) <- stationCovariateCols
    }
    secr::covariates(secr.traps) <- stationCovsDF
  }

  ###########
  # make capthist object
  capthist.secr <- make.capthist(captures = sdh2,
                                 traps    = secr.traps,
                                 fmt      = "trapID")

if(hasArg(makeRMarkInput)){
  if(isTRUE(makeRMarkInput)){
    RMarkDataframe <- RMarkInput(capthist.secr, covariates = TRUE)
    return(RMarkDataframe)
  }
}

  return(capthist.secr)
}