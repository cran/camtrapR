recordDatabase <- function(inDir,
                           includeCameras = FALSE,
                           exclude,
                           minDeltaTime = 0,
                           timeZone,
                           writecsv = FALSE,
                           outDir,
                           metadataTags
)
{
  stationCol <- "Station"
  speciesCol <- "Species"

  # check input
  if(hasArg(timeZone) == FALSE) {
    print("timeZone is not specified. Assuming UTC")
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }
  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")

  if(hasArg(includeCameras)){
    if(class(includeCameras) != "logical"){print("includeCameras must be of class 'logical'")}
    cameraCol <- "Camera"
  }
  if(hasArg(includeCameras) == FALSE){
    includeCameras = FALSE}

  if(hasArg(outDir)){
    if(class(outDir) != "character"){print("outDir must be of class 'character'")}
    if(file.exists(outDir) == FALSE) stop("outDir does not exist")
  }

  if(hasArg(exclude)){
    if(class(exclude) != "character"){print("exclude must be of class 'character'")}
  }
  if(hasArg(metadataTags)){
    if(class(metadataTags) != "character"){print("metadataTags must be of class 'character'")}
  }

  minDeltaTime <- as.integer(minDeltaTime)
  stopifnot(class(minDeltaTime) == "integer")
  stopifnot(class(writecsv) == "logical")

  if(class(inDir) != "character"){stop("inDir must be of class 'character'")}
  if(length(inDir) != 1){stop("inDir may only consist of 1 element only")}
  if(file.exists(inDir) == FALSE){stop("inDir does not exist")}


  ######
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  record.table <- data.frame(stringsAsFactors = FALSE)

  for(i in 1:length(dirs)){

    # create command line and execute exiftool
    if(hasArg(metadataTags)){
      command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal',paste(" -",metadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs[i], '"', sep = "")
      colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", metadataTags)
    } else {
      command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
      colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")
    }

    # some day, include the following (is faster, invokes Exiftool only once).
    # particularly useful in many folders with few images
    # no loop necessary!

    #         if(hasArg(metadataTags)){
    #           command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal', paste(" -",metadataTags,  collapse = "", sep = ""), ' -ext JPG "', paste(dirs, collapse = '" "'), '"', sep = "")
    #         } else {
    #           command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', paste(dirs, collapse = '" "'), '"', sep = "")
    #         }

    tmp1 <-  strsplit(system(command.tmp, intern=TRUE), split = "\t")

    metadata.tmp <- as.data.frame(matrix(unlist(lapply(tmp1, FUN = function(X){X[2]})),
                                         ncol = length(colnames.tmp),
                                         byrow = TRUE))

    colnames(metadata.tmp) <- colnames.tmp
    colnames(metadata.tmp) <- gsub(pattern = "[[:blank:]]", replacement = "", x = colnames(metadata.tmp))
    colnames(metadata.tmp) <- gsub(pattern = "[[:punct:]]", replacement = "", x = colnames(metadata.tmp))
    rm(colnames.tmp, tmp1)

    if(length(metadata.tmp) == 0){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      print(paste(dirs[i], "seems to contain no images;", " found", length.tmp, "jpgs"))

    } else {

      print(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))

      filenames.by.folder <- lapply(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE),
                                    FUN = list.files, pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE)
      names(filenames.by.folder) <- list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)

      metadata.tmp[,speciesCol] <-  gsub("/", "", rep(unlist(lapply(strsplit(names(filenames.by.folder), split = dirs[i], fixed = TRUE),
                                                                    FUN = function(X){X[[2]]})),
                                                      times = lapply(filenames.by.folder, length)))
      rm(filenames.by.folder)

      # convert character vector to time object and format for outfilename
      metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

      # add station and camera id (taken from photo filename. Make sure renaming is correct)
      if(isTRUE(includeCameras)){
        metadata.tmp <- cbind(metadata.tmp,
                              dirs_short[i],
                              sapply(strsplit(as.character(metadata.tmp$FileName), split = "__"), FUN = function(X){X[2]})
        )
        colnames(metadata.tmp)[ncol(metadata.tmp) - 1] <- stationCol
        colnames(metadata.tmp)[ncol(metadata.tmp)] <- cameraCol
      } else {
        metadata.tmp <- cbind(metadata.tmp,
                              dirs_short[i])
        colnames(metadata.tmp)[ncol(metadata.tmp)] <- stationCol
      }

      # remove species in "excluded"
      if(hasArg (exclude)){
        if(length(which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude) == TRUE)) > 0) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }
      }

      if(nrow(metadata.tmp) >= 1){   # if anything left, do

        # prepare to add time difference between observations columns
        metadata.tmp2 <- data.frame(metadata.tmp,
                                    delta.time.secs = NA,
                                    delta.time.mins = NA,
                                    delta.time.hours = NA,
                                    delta.time.days = NA)

        for(xy in 1:nrow(metadata.tmp2)){
          # time difference to all other records of same species at this station (at both cameras)
          diff_tmp <- na.omit(difftime(metadata.tmp2$DateTimeOriginal[xy], metadata.tmp2$DateTimeOriginal[metadata.tmp2[,speciesCol] == metadata.tmp2[xy,speciesCol]],
                                       units = "secs"))

          if(length(diff_tmp) == 0){   # i.e. no other record
            metadata.tmp2$delta.time.secs[xy] <-  NA
          }
          if(length(diff_tmp) >= 1 & length(diff_tmp[diff_tmp > 0]) == 0){ # i.e. 1st record
            metadata.tmp2$delta.time.secs[xy] <-  0
          }
          if(length(diff_tmp) >= 1 & length(diff_tmp[diff_tmp > 0]) > 0){ # i.e. not 1st record
            metadata.tmp2$delta.time.secs[xy] <- min(diff_tmp[diff_tmp > 0])
          }
        }
        # keep only records that meet these conditions:
        # 0 = first image of species
        # >= minDeltaTime = more than 1 hour (or whatever you set) difference
        # is.na = only 1 image of that species

        d1 <- metadata.tmp2[metadata.tmp2$delta.time.secs == 0 |
                              metadata.tmp2$delta.time.secs >= (minDeltaTime * 60) |
                              is.na(metadata.tmp2$delta.time.secs),]

        # remove duplicates (if 2 images taken in same second of same species at one station, remove all but first)
        d2 <- data.frame()
        for(spec in unique(d1[,speciesCol])){
          d1a <- subset(d1, d1[,speciesCol] == spec)
          row_to_remove <- which(duplicated(d1a[d1a[,speciesCol] == spec,]$DateTimeOriginal))
          if(length(row_to_remove) >= 1){
            d1b <- d1a[-row_to_remove,]
            d2 <- rbind(d2, d1b)
            rm(d1b)
          } else {
            d2 <- rbind(d2, d1a)
          }
          rm(d1a, row_to_remove)
        }
        record.table <- rbind(record.table, d2)
        rm(d1, d2)
      }
    }
  }

  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Maybe you excluded too many species?"))
  }

  # rearrange table, add date and time as separate columns

  record.table2  <-  data.frame(record.table[,c(stationCol,
                                                speciesCol, "DateTimeOriginal")],
                                Date = as.Date(record.table$DateTimeOriginal, tz = timeZone, format = "%Y/%M/%d"),
                                Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S",tz = timeZone),
                                record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
                                                "Directory", "FileName")])

  if(hasArg(metadataTags)){
    record.table2 <- cbind(record.table2, record.table[,metadataTags])
    colnames(record.table2)[(ncol(record.table2) - length(metadataTags) + 1) : ncol(record.table2)] <- metadataTags
  }

  if(isTRUE(includeCameras)){
    record.table2 <- data.frame(record.table2[,stationCol],
                                camera = record.table[,cameraCol],
                                record.table2[,2:ncol(record.table2)])
    colnames(record.table2)[1] <- stationCol
  }

  record.table2 <- record.table2[with(record.table2, order(record.table2[,stationCol], record.table2[,speciesCol], DateTimeOriginal)), ]

  # add delta time in hours and days
  record.table2$delta.time.secs <- round(record.table2$delta.time.secs, digits = 0)
  record.table2$delta.time.mins <- round(record.table2$delta.time.secs / 60, digits = 0)
  record.table2$delta.time.hours <- round(record.table2$delta.time.mins/60, digits = 1)
  record.table2$delta.time.days <- round(record.table2$delta.time.mins/60/24, digits = 1)

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_database_", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
      write.csv(record.table2, file = outtable_filename)
    } else {

      setwd(outDir)
      write.csv(record.table2, file = outtable_filename)
    }
  }
  return(record.table2)
}