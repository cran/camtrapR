surveyReport <- function(recordTable,
                         CTtable,
                         speciesCol = "Species",
                         stationCol = "Station",
                         cameraCol,
                         setupCol,
                         retrievalCol,
                         CTDateFormat = "%Y-%m-%d",
                         CTHasProblems = FALSE,
                         recordDateTimeCol = "DateTimeOriginal",
                         recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                         sinkpath
){
  stopifnot(c(stationCol, setupCol, retrievalCol) %in% colnames(CTtable))
  stopifnot(c(stationCol, recordDateTimeCol, stationCol) %in% colnames(recordTable))

  if(hasArg(cameraCol)){
    if(cameraCol %in% colnames(CTtable) == FALSE) stop(paste(cameraCol, "is not a column of CTtable"))

    camOp <- cameraOperation(CTtable = CTtable,
                             stationCol = stationCol ,
                             cameraCol = cameraCol,
                             setupCol = setupCol,
                             retrievalCol = retrievalCol,
                             writecsv = FALSE,
                             hasProblems = CTHasProblems,
                             allCamsOn = FALSE,
                             byCamera = FALSE,
                             dateFormat = CTDateFormat,
                             sumUpCameras = TRUE
    )

    camOp_for_trapnights <- cameraOperation(CTtable = CTtable,
                                            stationCol = stationCol ,
                                            cameraCol = cameraCol,
                                            setupCol = setupCol,
                                            retrievalCol = retrievalCol,
                                            writecsv = FALSE,
                                            hasProblems = CTHasProblems,
                                            allCamsOn = FALSE,
                                            byCamera = TRUE,
                                            dateFormat = CTDateFormat,
                                            sumUpCameras = TRUE
    )

    camOp_for_trapnights_no_prob <- cameraOperation(CTtable = CTtable,
                                                    stationCol = stationCol ,
                                                    cameraCol = cameraCol,
                                                    setupCol = setupCol,
                                                    retrievalCol = retrievalCol,
                                                    writecsv = FALSE,
                                                    hasProblems = FALSE,
                                                    allCamsOn = FALSE,
                                                    byCamera = TRUE,
                                                    dateFormat = CTDateFormat,
                                                    sumUpCameras = TRUE
    )

  } else {
    camOp <- cameraOperation(CTtable = CTtable,
                             stationCol =   stationCol ,
                             setupCol = setupCol,
                             retrievalCol = retrievalCol,
                             writecsv = FALSE,
                             hasProblems = CTHasProblems,
                             allCamsOn = FALSE,
                             byCamera = FALSE,
                             dateFormat = CTDateFormat,
                             sumUpCameras = FALSE
    )

    camOp_for_trapnights_no_prob <- cameraOperation(CTtable = CTtable,
                                                    stationCol =   stationCol ,
                                                    setupCol = setupCol,
                                                    retrievalCol = retrievalCol,
                                                    writecsv = FALSE,
                                                    hasProblems = FALSE,
                                                    allCamsOn = FALSE,
                                                    byCamera = FALSE,
                                                    dateFormat = CTDateFormat,
                                                    sumUpCameras = FALSE
    )
  }

  recordTable$DateTime2 <- strptime(recordTable[,recordDateTimeCol],
                                    format = recordDateTimeFormat)
  recordTable$Date2 <- as.Date(recordTable$DateTime2)

  recordTable[,speciesCol] <- as.character(recordTable[,speciesCol])
  recordTable[,stationCol] <- as.character(recordTable[,stationCol])

  if("POSIXlt" %in% class(recordTable$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(recordTable$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(recordTable$DateTime2)), collapse = ", ")))

  # adjust options for printing results
  options.tmp <- options()
  on.exit(options(options.tmp))
  options(max.print=1e6)
  options(width = 1000)

  if(all(as.character(unique(recordTable[,stationCol])) %in% rownames(camOp)) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by rownames of camOp"))
  }

  # remove rows with only NAs (station was never set up)
  remove.rows.tmp <- which(rowSums(is.na(camOp)) == ncol(camOp))
  if(length(remove.rows.tmp) >= 1){
    camOp2 <- camOp[-remove.rows.tmp,]    # remove rows with only NAs
  } else {
    camOp2 <- camOp
  }
  rm(remove.rows.tmp)

  # some survey information
  sinkfile <- paste("survey_report_", Sys.Date(), ".txt", sep = "")

  if(hasArg(sinkpath)){
    setwd(sinkpath)
    sink(file = sinkfile)
    print(paste("Survey Report generated", Sys.Date() ))
  }
  cat("\n-------------------------------------------------------\n")
  print(paste("Total number of stations: ", nrow(camOp)))
  cat("\n-------------------------------------------------------\n")
  print(paste("Number of operational stations: ", nrow(camOp2)))
  cat("\n-------------------------------------------------------\n")

  if(hasArg(cameraCol)){
    print(paste("Total number of cameras: ", length(unique(paste(CTtable$Station, CTtable$Camera, sep = "_")))))
    cat("\n-------------------------------------------------------\n")
    print(paste("n nights with cameras set up (operational or not): ",
                sum(!is.na(camOp_for_trapnights_no_prob)) - nrow(camOp_for_trapnights_no_prob)))
    cat("\n-------------------------------------------------------\n")
    print(paste("n nights with cameras set up (trap nights): ",
                sum(camOp_for_trapnights == 1, na.rm = TRUE) - nrow(camOp_for_trapnights)))
  } else {
    print(paste("n nights with cameras set up (operational or not. NOTE: only correct if 1 camera per station):",
                sum(!is.na(camOp_for_trapnights_no_prob)) - nrow(camOp_for_trapnights_no_prob)))
    cat("\n-------------------------------------------------------\n")
    print(paste("n nights with cameras set up  (trap nights. NOTE: only correct if 1 camera per station):",
                sum(camOp2 == 1, na.rm = TRUE) - nrow(camOp2)))
  }
  cat("\n-------------------------------------------------------\n")
  print(paste("total trapping period: ", paste(range(colnames(camOp2)), collapse = " - ")))

  ###
  station.tmp1 <-  aggregate(CTtable[,setupCol],
                             list(CTtable[,stationCol]),
                             FUN = function(X){min(as.Date(strptime(X, format = CTDateFormat)))})
  station.tmp2 <- aggregate(CTtable[,retrievalCol],
                            list(CTtable[,stationCol]),
                            FUN = function(X){max(as.Date(strptime(X, format = CTDateFormat)))})
  image.tmp1 <-  aggregate(recordTable$Date2,
                           list(recordTable[,stationCol]),
                           FUN = min)
  image.tmp2 <- aggregate(recordTable$Date2,
                          list(recordTable[,stationCol]),
                          FUN = max)

  date_range_combined <- data.frame(station.tmp1[,1], station.tmp1[,2],
                                    image.tmp1[match(station.tmp1[,1], image.tmp1[,1]),2],
                                    image.tmp2[match(station.tmp1[,1], image.tmp2[,1]),2],
                                    station.tmp2[,2],
                                    rowSums(!is.na(camOp2)) - 1,
                                    rowSums(camOp2 == 1, na.rm = TRUE) - 1)   # )   #
  colnames(date_range_combined) <- c(stationCol, "setup_date",  "first_image_date", "last_image_date", "retrieval_date", "n_total_days", "n_active_days")
  rownames(date_range_combined) <- NULL

  # total number of independent records by species
  species_record_table <- data.frame(species = NA, n_events = NA, n_stations = NA)

  for(i in 1:length(unique(recordTable[, speciesCol]))){

    tmp <- unique(recordTable[, speciesCol])[i]
    subset.tmp <- subset(recordTable, recordTable[, speciesCol] == tmp)
    species_record_table[i, ] <- c(tmp, nrow(subset.tmp), length(unique(subset.tmp[,stationCol])))
    rm(subset.tmp, tmp)
  }
  species_record_table2 <- species_record_table[order(species_record_table$species),]
  rownames(species_record_table2) <- NULL

  # total number of independent records by station

  # only species that were recorded
  station_record_table1 <- aggregate(recordTable[,1], by = list(recordTable[,stationCol],recordTable[,speciesCol]), FUN = length)
  colnames(station_record_table1) <- c(stationCol, speciesCol, "n_events")
  station_record_table1 <- station_record_table1[order(station_record_table1[,stationCol], station_record_table1[,speciesCol]),]
  rownames(station_record_table1) <- NULL

  #including all species and 0s
  station_record_table <- expand.grid(sort(unique(recordTable[,stationCol])), sort(unique(recordTable[,speciesCol])))
  station_record_table <- data.frame(station_record_table, n_events = 0)
  colnames(station_record_table) <- c(stationCol, speciesCol, "n_events")
  rownames(station_record_table) <- NULL
  # species lists by station

  n_spec_by_station  <- aggregate(station_record_table1[,speciesCol], by = list(station_record_table1[,stationCol]), FUN = length)
  colnames(n_spec_by_station) <- c(stationCol, "n_species")
  rownames(n_spec_by_station) <- NULL

  for(i in 1:length(unique(recordTable[, stationCol]))){

    tmp <- unique(recordTable[, stationCol])[i]
    subset.tmp <- table(subset(recordTable, recordTable[, stationCol] == tmp)[,speciesCol] )

    station_record_table.tmp <- station_record_table[station_record_table[, stationCol] == tmp,]
    station_record_table.tmp$n_events[match(names(subset.tmp), station_record_table.tmp$Species)] <- subset.tmp

    station_record_table[station_record_table[, stationCol] == tmp,] <- station_record_table.tmp
    rm(station_record_table.tmp)
  }
  station_record_table2 <-  station_record_table[order(station_record_table[,stationCol], station_record_table[,speciesCol]),]
  rownames(station_record_table2) <- NULL

  if(hasArg(sinkpath)){
    cat("\n\n-------------------------------------------------------\n\n")
    print(" survey station and image date ranges")
    print(date_range_combined)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of species by station")
    print(n_spec_by_station)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and station by species")
    print(species_record_table2)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and species by station (only species that were recorded at stations)")
    print(station_record_table1)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and species by station (all species, all stations, including species that were not recorded)")
    print(station_record_table2)
    sink()
    cat("saved output to file \n",
        paste(sinkpath, sinkfile, sep = "/"))
  }

  output <- list(date_range_combined, n_spec_by_station, species_record_table2, station_record_table1, station_record_table2)
  names(output) <- c("survey_dates", "species_by_station", "events_by_species",
                     "events_by_station", "events_by_station2")
  return(output)
}
