detectionHistory <- function(recordTable,
                             species,
                             camOp,
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
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
                             outDir)
{

  #################
  # check input

  stopifnot(hasArg(species))
  stopifnot(is.character(species))
  stopifnot(length(species) == 1)

  stopifnot(hasArg(sessionLength))

  stopifnot(hasArg(stationCol))
  stopifnot(is.character(stationCol))
  stopifnot(length(stationCol) == 1)

  stopifnot(hasArg(speciesCol))
  stopifnot(is.character(speciesCol))
  stopifnot(length(speciesCol) == 1)

  stopifnot(hasArg(recordDateTimeCol))
  stopifnot(is.character(recordDateTimeCol))
  stopifnot(length(recordDateTimeCol) == 1)

  stopifnot(hasArg(recordTable))
  stopifnot(hasArg(camOp))

  if(hasArg(timeZone) == FALSE) {
    print("timeZone is not specified. Assuming UTC")
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }
  stopifnot(is.logical(writecsv))

  SessionStartTime <- as.integer(round(SessionStartTime))
  if(SessionStartTime != 0 & !is.integer(SessionStartTime)) {stop ("SessionStartTime must be between 0 and 23")}
  if(SessionStartTime < 0 | SessionStartTime >= 24){stop ("SessionStartTime must be between 0 and 23")}

  sessionLength <- as.integer(round(sessionLength))
  if(sessionLength <= 0) stop("sessionLength must be a positive integer and not 0")
  if(sessionLength > ncol(camOp)/2) stop("sessionLength may not be greater than half the total number of days in camOp")
  stopifnot(is.numeric(sessionLength))

  if(hasArg(maxNumberDays)){
    maxNumberDays <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp)) stop("maxNumberDays must be smaller than the number of columns of camOp")
    if(maxNumberDays < sessionLength) stop("maxNumberDays must be larger than or equal to sessionLength")
  }

  stopifnot(c(speciesCol, recordDateTimeCol, stationCol) %in% colnames(recordTable))

  if(species %in% recordTable[,speciesCol] == FALSE) stop("species is not in speciesCol of recordTable")

  if(writecsv == TRUE){
    if(file.exists(outDir) == FALSE){stop("outDir does not exist")}
  }

  if(hasArg(minimumEffort)){
    stopifnot(is.numeric(minimumEffort))
    minimumEffort <- as.integer(minimumEffort)
  }

  if(includeEffort == TRUE){
    if(hasArg(scaleEffort) == FALSE) stop("scaleEffort must be defined if includeEffort is TRUE")
    if(class(scaleEffort) != "logical") stop("scaleEffort must be logical (TRUE or FALSE)")
  }

  camOp.mat <- as.matrix(camOp)

  #############
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
  rec.tmp.min  <- tapply(subset_species$Date2, INDEX = subset_species[,stationCol], FUN = min)
  rec.tmp.max  <- tapply(subset_species$Date2, INDEX = subset_species[,stationCol], FUN = max)

  d <- data.frame(rec.min = as.Date(rec.tmp.min, origin = "1970-01-01"),
                  rec.max = as.Date(rec.tmp.max, origin = "1970-01-01"),
                  cam.min = as.Date(cam.range.tmp[match(names(rec.tmp.max), rownames(cam.range.tmp)),1]),
                  cam.max = as.Date(cam.range.tmp[match(names(rec.tmp.max), rownames(cam.range.tmp)),2])
  )
  rownames(d) <- names(rec.tmp.min)

  if(any(d$rec.min < d$cam.min | d$rec.max > d$cam.max, na.rm = TRUE)) stop(paste("record date outside camera operation date range: ",
                                                                                  paste(rownames(d)[which(d$rec.min < d$cam.min)], collapse = ", " )))
  rm(cam.range.tmp, rec.tmp.min, rec.tmp.max, d)

  ################################################
  # add records by day to matrix

  record.hist <- matrix(NA, nrow = nrow(camOp.mat), ncol = ncol(camOp.mat))
  record.hist[which(!is.na(camOp.mat))] <- 0
  rownames(record.hist) <- rownames(camOp.mat)
  colnames(record.hist) <- colnames(camOp.mat)

  record.by.station.tmp <- tapply(subset_species$DateTime2 - (SessionStartTime * 3600),
                                  INDEX = subset_species[,stationCol],
                                  FUN =  function(X){unique(as.Date(X, tz = timeZone))})

  for(i in 1:length(record.by.station.tmp)){
    record.hist[match(names(record.by.station.tmp[i]), rownames(record.hist)),
                match(as.character(record.by.station.tmp[[i]]),  colnames(record.hist))] <- 1
  }
  rm(i)

  cam.op.worked <- camOp.mat

  if(hasArg(maxNumberDays)){
    rec.min.tmp <- apply(record.hist, MARGIN = 1, function(X){min(which(!is.na(X)))})
    for(j in 1:nrow(record.hist)){
      if(rec.min.tmp[j]+maxNumberDays <= ncol(cam.op.worked)){
        record.hist[j,seq(rec.min.tmp[j]+maxNumberDays,ncol(cam.op.worked),1)] <- NA
        cam.op.worked[j,seq(rec.min.tmp[j]+maxNumberDays,ncol(cam.op.worked),1)] <- NA
      }
    }
    rm(j, rec.min.tmp)
  }

  # adjust camera operation matrix if SessionStartTime != 0
  if(SessionStartTime != 0){

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

  colnames(record.hist) <- paste(colnames(record.hist), "+",SessionStartTime, "h", sep = "" )
  colnames(cam.op.worked) <- paste(colnames(cam.op.worked), "+", SessionStartTime, "h", sep = "")

  ################################################
  # if beginning with day one, remove leading NAs for each station

  if(isTRUE(BeginWithDay1)){
    rec.min.tmp <- apply(record.hist, MARGIN = 1, function(X){min(which(!is.na(X)))})    # 1st day of each station
    rec.max.tmp <- apply(record.hist, MARGIN = 1, function(X){max(which(!is.na(X)))})    # last day of each station
    diff.days.tmp <- rec.max.tmp - rec.min.tmp

    record.hist2 <- matrix(NA,
                           nrow = nrow(record.hist),
                           ncol = max(diff.days.tmp)+1)
    cam.op2 <- matrix(NA,
                      nrow = nrow(camOp.mat),
                      ncol = max(diff.days.tmp)+1)

    for(l in 1:nrow(record.hist)){
      record.hist2[l,1:(diff.days.tmp[l]+1)] <- record.hist[l,rec.min.tmp[l]:rec.max.tmp[l]]
      cam.op2[l,1:(diff.days.tmp[l]+1)] <- as.vector(camOp.mat[l,rec.min.tmp[l]:rec.max.tmp[l]])
    }
    colnames(record.hist2) <- colnames(cam.op2) <- paste("day", 1:ncol(record.hist2), sep = "")
    record.hist <- record.hist2
    cam.op.worked <- cam.op2
    rm(rec.min.tmp, rec.max.tmp, diff.days.tmp, record.hist2)
  }

  ################################################
  # aggregate by sessions

  if(ncol(record.hist) != ncol(cam.op.worked))stop("Hi! I'm a weird and unexpected bug. Please report me.")

  if(sessionLength == 1){

    record.hist3 <- record.hist
    cam.op3 <- cam.op.worked
    effort <- cam.op3          # if sessionLength = 1 day, it is identical

  } else {

    record.hist3 <- matrix(NA, nrow = nrow(record.hist), ncol = ceiling(ncol(record.hist) / sessionLength ))
    cam.op3 <- matrix(NA, nrow = nrow(cam.op.worked), ncol = ceiling(ncol(cam.op.worked) / sessionLength ))
    effort <- matrix(NA, nrow = nrow(cam.op.worked), ncol = ceiling(ncol(cam.op.worked) / sessionLength ))

    index <- 1

    for(m in 1:(ncol(record.hist3))){

      if(index + sessionLength <= ncol(record.hist)){
        index.tmp <- index : (index + sessionLength - 1)
      } else {
        index.tmp <- index : ncol(record.hist)
      }

      if(isTRUE(includeEffort)){    # effort = TRUE

        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = sum, na.rm = TRUE) >= 1), m] <- 1
        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = sum, na.rm = TRUE) == 0), m] <- 0
        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = function(X){all(is.na(X))})), m] <- NA   # if all NA in session (cams not set up)

        effort[, m] <- apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = sum, na.rm = TRUE)
        effort[which(apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = function(X){all(is.na(X))})), m] <- NA   # if all NA in session (cams not set up)

        if(hasArg(minimumEffort)){
          record.hist3[which(effort[,m] < minimumEffort), m] <- NA
        }
      } else {           # effort = FALSE

        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = sum) >= 1), m] <- 1
        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = sum) == 0), m] <- 0
        record.hist3[which(apply(as.matrix(record.hist[,index.tmp]), MARGIN = 1, FUN = function(X){any(is.na(X))})), m] <- NA  # if al least 1 NA in session

        # camOp = 1 if camera was operational all days of session.
        cam.op3[which(apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = sum) == sessionLength), m] <- 1
        cam.op3[which(apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = sum) < sessionLength), m] <- 0
        cam.op3[which(apply(as.matrix(cam.op.worked[,index.tmp]), MARGIN = 1, FUN = function(X){any(is.na(X))})), m] <- NA

        record.hist3[which(cam.op3[,m] == 0),m] <- NA     # set record history NA where cam.op = 0 (not operational)
      }
      index <- index + sessionLength
    }
    rm(index, index.tmp)
  }
       
  if(isTRUE(includeEffort)){
      if(isTRUE(scaleEffort)){      
        if(sessionLength == 1) stop("cannot scale effort if sessionLength is 1")
        scale.eff.tmp <- scale(as.vector(effort))
        scale.eff.tmp.attr <- data.frame(effort.scaled.center = NA,
                                         effort.scaled.scale = NA)
        scale.eff.tmp.attr$effort.scaled.center[1] <- attr(scale.eff.tmp, which = "scaled:center")
        scale.eff.tmp.attr$effort.scaled.scale[1] <- attr(scale.eff.tmp, which = "scaled:scale")
        effort <- matrix(scale.eff.tmp, nrow = nrow(effort), ncol = ncol(effort))
      }
    }
    

    
  ################################################
  # rownames of output

  row.names(record.hist3) <-  rownames(camOp.mat)
  if(isTRUE(includeEffort))  {row.names(effort)  <-  rownames(camOp.mat)}

  ################################################
  # column names for output table

  if(isTRUE(DatesAsSessionNames)){
    if(sessionLength == 1){
      colnames.tmp <- colnames(cam.op.worked)
    } else {
      seq.tmp <- seq(from = 1, by = sessionLength, length.out = ncol(record.hist3))
      colnames.tmp <- paste(colnames(cam.op.worked)[seq.tmp],
                            colnames(cam.op.worked)[seq.tmp + sessionLength - 1], sep = "_")
      colnames.tmp[length(colnames.tmp)] <- paste(colnames(cam.op.worked)[max(seq.tmp)],
                                                  colnames(cam.op.worked)[ncol(cam.op.worked)],
                                                  sep = "_")
    }
    colnames(record.hist3) <- colnames(cam.op3)  <- colnames.tmp
    if(includeEffort == TRUE) {colnames(effort) <- colnames.tmp}
  }  else {
    colnames(record.hist3) <-  paste("s", seq(1,ncol(record.hist3), by = 1),sep = "")
    if(includeEffort == TRUE) {colnames(effort) <-  paste("s", seq(1,ncol(effort), by = 1), sep = "")}
  }

  ################################################
  # save output as table
  day1string <- ifelse(isTRUE(BeginWithDay1), "from_1st_day", "by_date")
  effortstring <- ifelse(isTRUE(includeEffort), "with_effort__", "no_effort__")
  maxNumberDaysstring <- ifelse(hasArg(maxNumberDays), paste("max",maxNumberDays,"days_", sep = ""), "")
  if(hasArg(scaleEffort)){
    scaleEffortstring <- ifelse(isTRUE(scaleEffort), paste("scaled_"), "not_scaled_")
  } else {
    scaleEffortstring <- ""
  }

  outtable.name <- paste(species, "__record_history__", effortstring,
                         sessionLength, "_days_per_session_",
                         maxNumberDaysstring,
                         SessionStartTime,"h_",
                         day1string, "_",
                         Sys.Date(),
                         ".csv", sep = "")

  outtable.name.effort <- paste(species, "__effort__",
                                scaleEffortstring,
                                sessionLength, "_days_per_session_",
                                maxNumberDaysstring,
                                SessionStartTime,"h_",
                                day1string, "_",
                                Sys.Date(),
                                ".csv", sep = "")

  outtable.name.effort.scale <- paste(species, "__effort_scaling_parameters__",
                                      sessionLength, "_days_per_session_",
                                      maxNumberDaysstring,
                                      SessionStartTime,"h_",
                                      day1string, "_",
                                      Sys.Date(),
                                      ".csv", sep = "")

  if(isTRUE(writecsv)){
    setwd(outDir)
    write.csv(record.hist3, file = outtable.name)
    if(isTRUE(includeEffort)){
      write.csv(effort, file = outtable.name.effort)
      if(hasArg(scaleEffort)){
        if(scaleEffort == TRUE)  write.csv(scale.eff.tmp.attr, file = outtable.name.effort.scale)
      }
    }
  }
  if(isTRUE(includeEffort)){
    if(scaleEffort == TRUE){
      return(list(record.hist3, effort, scale.eff.tmp.attr))
    } else {
      return(list(record.hist3, effort))
    }
  } else {
    return(list(record.hist3))
  }
}