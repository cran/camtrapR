spatialDetectionHistory <- function(recordTable,
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
                                    recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                    occasionLength,
                                    occasionStartTime = 0,
                                    maxNumberDays,
                                    beginWithDay1,
                                    includeEffort,
                                    scaleEffort,
                                    binaryEffort,
                                    timeZone
)
{

  #################
  # check input


  stopifnot(hasArg(species))
  stopifnot(is.character(species))
  stopifnot(length(species) == 1)

  stopifnot(hasArg(occasionLength))

  stopifnot(hasArg(stationCol))
  stopifnot(is.character(stationCol))
  stopifnot(length(stationCol) == 1)

  stopifnot(hasArg(speciesCol))
  stopifnot(is.character(speciesCol))
  stopifnot(length(speciesCol) == 1)

  stopifnot(hasArg(individualCol))
  stopifnot(is.character(individualCol))
  stopifnot(length(individualCol) == 1)

  stopifnot(hasArg(recordDateTimeCol))
  stopifnot(is.character(recordDateTimeCol))
  stopifnot(length(recordDateTimeCol) == 1)

  stopifnot(hasArg(recordTable))
  stopifnot(hasArg(camOp))

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
  #stopifnot(is.logical(writecsv))

  occasionStartTime <- as.integer(round(occasionStartTime))
  if(occasionStartTime != 0 & !is.integer(occasionStartTime)) {stop ("occasionStartTime must be between 0 and 23")}
  if(occasionStartTime < 0 | occasionStartTime >= 24){stop ("occasionStartTime must be between 0 and 23")}

  occasionLength <- as.integer(round(occasionLength))
  if(occasionLength <= 0) stop("occasionLength must be a positive integer and not 0")
  if(occasionLength > ncol(camOp)/2) stop("occasionLength may not be greater than half the total number of days in camOp")
  stopifnot(is.numeric(occasionLength))

  if(hasArg(maxNumberDays)){
    maxNumberDays <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp)) stop("maxNumberDays must be smaller than the number of columns of camOp")
    if(maxNumberDays < occasionLength) stop("maxNumberDays must be larger than or equal to occasionLength")
  }

  stopifnot(c(speciesCol, recordDateTimeCol, stationCol, individualCol) %in% colnames(recordTable))
  if(species %in% recordTable[,speciesCol] == FALSE) stop("species is not in speciesCol of recordTable")

  # check all stations in recordTable are matched in CTtable
  if(all(recordTable[,stationCol] %in% CTtable[,stationCol]) == FALSE) {
    stop(paste("items of stationCol in recordTable are not matched in stationCol of CTtable: ", paste(recordTable[-which(recordTable[,stationCol] %in% CTtable[,stationCol]),stationCol], collapse = ", ")))
  }


  if(includeEffort == TRUE){
    if(hasArg(scaleEffort) == FALSE & hasArg(binaryEffort) == FALSE) {scaleEffort <- binaryEffort <- FALSE}
    if(hasArg(scaleEffort)){
      if(class(scaleEffort) != "logical") stop("scaleEffort must be logical (TRUE or FALSE)")
    } else {
      scaleEffort <- FALSE
    }
    if(hasArg(binaryEffort)){
      if(class(binaryEffort) != "logical") stop("binaryEffort must be logical (TRUE or FALSE)")
    } else {
      binaryEffort <- FALSE
    }
    if(binaryEffort == TRUE & scaleEffort == TRUE) stop("'scaleEffort' and 'binaryEffort' cannot both be TRUE")
  }

  camOp.mat <- as.matrix(camOp)


#####################################################################################################################

  # bring date, time, station ids into shape

  subset_species <- subset(recordTable, recordTable[,speciesCol] == species)

  subset_species$DateTime2 <- strptime(subset_species[,recordDateTimeCol], tz = timeZone, format = recordDateTimeFormat)
  subset_species$Date2 <- as.Date(subset_species$DateTime2)

  if("POSIXlt" %in% class(subset_species$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(subset_species$DateTime2))) stop("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat")

  if(all(as.character(unique(subset_species[,stationCol])) %in% rownames(camOp.mat)) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by rownames of camOp"))
  }

  ################################################
  # check if images were taken between setup and retrieval dates (Error if images outside station date range)

  cam.range.tmp <-  data.frame(t(apply(camOp.mat, MARGIN = 1, FUN = function(X){colnames(camOp.mat)[range(which(!is.na(X)))]})))
  rec.tmp.min  <- tapply(subset_species$Date2, INDEX = subset_species[,stationCol], FUN = min, simplify = FALSE)
  rec.tmp.max  <- tapply(subset_species$Date2, INDEX = subset_species[,stationCol], FUN = max, simplify = FALSE)

  d <- data.frame(rec.min = as.Date(unlist(rec.tmp.min), origin = "1970-01-01"),
                  rec.max = as.Date(unlist(rec.tmp.max), origin = "1970-01-01"),
                  cam.min = as.Date(cam.range.tmp[match(names(rec.tmp.min), rownames(cam.range.tmp)),1]),
                  cam.max = as.Date(cam.range.tmp[match(names(rec.tmp.max), rownames(cam.range.tmp)),2])
  )
  rownames(d) <- names(rec.tmp.min)

  if(any(d$rec.min < d$cam.min | d$rec.max > d$cam.max, na.rm = TRUE)) stop(paste("record date outside camera operation date range: ",
                                                                                  paste(rownames(d)[which(d$rec.min < d$cam.min)], collapse = ", " )))
  rm(cam.range.tmp, rec.tmp.min, rec.tmp.max)


  ###############
  cam.op.worked <- camOp.mat

  #   # if maxNumberDays is defined, cut off detection histories after n days

  if(hasArg(maxNumberDays)){
    rec.min.tmp <- apply(cam.op.worked, MARGIN = 1, function(X){min(which(!is.na(X)))})
    for(j in 1:nrow(cam.op.worked)){
      if(rec.min.tmp[j]+maxNumberDays <= ncol(cam.op.worked)){
        cam.op.worked[j,seq(rec.min.tmp[j] + maxNumberDays, ncol(cam.op.worked),1)] <- NA
      }
    }
    rm(j, rec.min.tmp)
  }

  # adjust camera operation matrix if occasionStartTime != 0
  if(occasionStartTime != 0){

    for(k in 1:nrow(cam.op.worked)){
      tmp <- as.vector(cam.op.worked[k,])
      # set last operational day of camOp NA
      tmp[max(which(!is.na(tmp)))]  <- NA
      # if station was not operational, set both affected days 0
      tmp[sort(unique(c(grep(pattern = 0, x= tmp), grep(pattern = 0, x= tmp)-1)))] <- 0
      # make sure no additional 0s were there used to be NAs (1st entry)
      tmp[which(is.na(as.vector(cam.op.worked[k,])))] <- NA
      cam.op.worked[k,]  <- tmp
      rm(tmp)
    }
  }

  colnames(cam.op.worked) <- paste(colnames(cam.op.worked), "+", occasionStartTime, "h", sep = "")

  ###
  # if beginning with day one, remove leading NAs for each station

  if(isTRUE(beginWithDay1)){
    cam.tmp.min <- apply(cam.op.worked, MARGIN = 1, function(X){min(which(!is.na(X)))})    # 1st day of each station
    cam.tmp.max <- apply(cam.op.worked, MARGIN = 1, function(X){max(which(!is.na(X)))})    # last day of each station

    diff.days.tmp <- cam.tmp.max - cam.tmp.min

    cam.op2 <- matrix(NA,
                      nrow = nrow(camOp.mat),
                      ncol = max(diff.days.tmp)+1)

    for(l in 1:nrow(camOp.mat)){
      cam.op2[l,1:(diff.days.tmp[l]+1)] <- as.vector(camOp.mat[l,cam.tmp.min[l]:cam.tmp.max[l]])
    }
    colnames(cam.op2) <- paste("day", 1:ncol(cam.op2), sep = "")
    cam.op.worked <- cam.op2
    rm(cam.tmp.min, cam.tmp.max, diff.days.tmp)
  }

  ####
  # time difference between records and day 1st camera was set up
  subset_species$occasion1 <- as.numeric(ceiling((difftime(time1  = subset_species$DateTimeOriginal,
                                                           time2 =  as.POSIXct(paste(as.character(min(d$cam.min)), "00:00:00"), tz = timeZone),
                                                           units = "secs",
                                                           tz = timeZone)
                                                  - occasionStartTime * 3600) / (occasionLength * 86400)))
  # time difference between records and their station's setup date
  subset_species$occasion2 <- as.numeric(ceiling((difftime(time1  = subset_species$DateTimeOriginal,
                                                           time2 = as.POSIXct(paste(as.character(d$cam.min[match(subset_species$Station, rownames(d))]), "00:00:00"), tz = timeZone),
                                                           units = "secs",
                                                           tz = timeZone)
                                                  - occasionStartTime * 3600) / (occasionLength * 86400)))

  occasionCol <- ifelse(beginWithDay1 == TRUE,  "occasion2",  "occasion1")


  # compute number of active days per occasion (effort)

  if(isTRUE(includeEffort)){
    if(occasionLength == 1){
      effort <- cam.op.worked          # if occasionLength = 1 day, it is identical
      effort <- ifelse(is.na(effort), 0, effort)
    } else {
      effort <- matrix(NA, nrow = nrow(cam.op.worked), ncol = ceiling(ncol(cam.op.worked) / occasionLength ))

      index <- 1
      for(m in 1:(ncol(effort))){
        # columns to aggregate
        if(index + occasionLength <= ncol(cam.op.worked)){
          index.tmp <- index : (index + occasionLength - 1)
        } else {
          index.tmp <- index : ncol(cam.op.worked)
        }

        effort[, m] <- apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = sum, na.rm = TRUE)
        #effort[which(apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = function(X){all(is.na(X))})), m] <- NA   # if all NA in occasion (cams not set up)
        index <- index + occasionLength
      }
      rm(index, index.tmp)
    }


    if(isTRUE(scaleEffort)){
      if(occasionLength == 1) stop("cannot scale effort if occasionLength is 1")
      scale.eff.tmp <- scale(as.vector(effort))
      scale.eff.tmp.attr <- data.frame(effort.scaled.center = NA,
                                       effort.scaled.scale = NA)
      scale.eff.tmp.attr$effort.scaled.center[1] <- attr(scale.eff.tmp, which = "scaled:center")
      scale.eff.tmp.attr$effort.scaled.scale[1] <- attr(scale.eff.tmp, which = "scaled:scale")
      effort <- matrix(scale.eff.tmp, nrow = nrow(effort), ncol = ncol(effort))
      print("scaling parameters")
      print(scale.eff.tmp.attr)
    }

    if(isTRUE(binaryEffort)){
      effort <- ifelse(effort >= 1, 1, 0)
    }

    rownames(effort) <- rownames(cam.op.worked)

    if(max(subset_species[,occasionCol]) > ncol(effort)) {stop("encountered a bug. Please report it.")}
  }

  # column names for effort matrix
  if(includeEffort == TRUE) {colnames(effort) <-  paste("o", seq(1,ncol(effort), by = 1), sep = "")}

  #############
  # build spatial detection history

  if(hasArg(individualCovariateCols)){
    sdh0 <- subset_species[, c(individualCol, occasionCol, stationCol, individualCovariateCols)]
    colnames(sdh0) <- c("ID", "Occasion", "trapID", individualCovariateCols)
  } else {
    sdh0 <- subset_species[, c(individualCol, occasionCol, stationCol)]
    colnames(sdh0) <- c("ID", "Occasion", "trapID")
  }

  # remove unidentified individuals
  remove.tmp <- which(is.na(sdh0[,"ID"]))
  if(length(remove.tmp) >= 1){
    sdh0 <- sdh0[-remove.tmp,]
    print(paste("removed", length(remove.tmp), "records because of missing individual IDs"))
  }

  sdh1 <- data.frame(Session = 1, sdh0)
  sdh2 <- unique(sdh1)

  ############
  # make secr traps object

  coord.ct <- CTtable[,c(Xcol, Ycol)]
  colnames(coord.ct) <- c("x", "y")
  rownames(coord.ct) <- CTtable[,stationCol]

  if(includeEffort == TRUE) {
    if(binaryEffort == TRUE){
      traps <- read.traps(data = coord.ct,
                               detector = "proximity",
                               binary.usage = TRUE)
    } else {
      secr.traps <- read.traps(data = coord.ct,
                               detector = "proximity",
                               binary.usage = FALSE)
    }
    secr::usage(secr.traps) <- effort
  } else {
    secr.traps <- read.traps(data = coord.ct,
                             detector = "proximity")
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
                                 traps = secr.traps,
                                 fmt = "trapID")


  return(capthist.secr)
}