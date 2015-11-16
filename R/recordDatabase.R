recordDatabase <- function(inDir,
                           hasSpeciesFolders = TRUE,
                           cameraID,
                           camerasIndependent,
                           exclude,
                           minDeltaTime = 0,
                           timeZone,
                           stationCol,
                           writecsv = FALSE,
                           outDir,
                           customMetadataTags,
                           metadataHierarchyDelimitor = "|",
                           metadataSpeciesTag,
                           additionalMetadataTags
)
{

  if(hasArg(stationCol) == FALSE) stationCol <- "Station"
  stopifnot(is.character(stationCol))
  speciesCol <- "Species"

  # check input
  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }
  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")

  if(!is.logical(hasSpeciesFolders)) stop("hasSpeciesFolders must be of class 'logical'")

  if(hasArg(metadataSpeciesTag)){
    if(class(metadataSpeciesTag) != "character"){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of lenght 1")}
  }

  if(hasArg(cameraID)){
    if(class(cameraID) != "character"){stop("cameraID must be of class 'character'")}
    if(cameraID %in% c("filename", "directory") == FALSE) {stop("cameraID can only be 'filename', 'directory', or missing")}
    if(class(camerasIndependent) != "logical"){stop("camerasIndependent must be of class 'logical'")}
  } else { camerasIndependent <- FALSE
  }
  cameraCol <- "Camera"


  if(hasArg(outDir)){
    if(class(outDir) != "character"){stop("outDir must be of class 'character'")}
    if(file.exists(outDir) == FALSE) stop("outDir does not exist")
  }

  if(hasArg(exclude)){
    if(class(exclude) != "character"){stop("exclude must be of class 'character'")}
  }

  if(hasArg(customMetadataTags)){
    if(class(customMetadataTags) != "logical"){stop("customMetadataTags must be of class 'logical'")}
    stopifnot(metadataHierarchyDelimitor %in% c("|", ":"))
  } else {customMetadataTags <- FALSE}

  if(hasArg(additionalMetadataTags)){
    if(class(additionalMetadataTags) != "character"){stop("additionalMetadataTags must be of class 'character'")}
  }


  minDeltaTime <- as.integer(minDeltaTime)
  stopifnot(class(minDeltaTime) == "integer")
  stopifnot(class(writecsv) == "logical")

  if(class(inDir) != "character"){stop("inDir must be of class 'character'")}
  if(length(inDir) != 1){stop("inDir may only consist of 1 element only")}
  if(file.exists(inDir) == FALSE){stop("inDir does not exist")}


  # find image directories
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  record.table <- data.frame(stringsAsFactors = FALSE)

  for(i in 1:length(dirs)){   # loop through station directories



    # check if specified metadata tags are present in images
    if(hasArg(additionalMetadataTags) & hasSpeciesFolders == TRUE){
      exiftags.tmp <- exifTagNames(inDir, whichSubDir = i)
      if(all(additionalMetadataTags %in% exiftags.tmp) == FALSE) {
        stop(paste("metadata tag was not found in the images:",
                   paste(additionalMetadataTags[which(additionalMetadataTags %in% exiftags.tmp == FALSE)], collapse = ", "),
                   list.files(dirs[i],
                              full.names = TRUE,
                              pattern = ".JPG$|.jpg$",
                              recursive = TRUE)[1], sep = "\n"))
      }
      rm(exiftags.tmp)
    }

    # create command line and execute exiftool
    if(customMetadataTags == TRUE){

      if(hasArg(additionalMetadataTags)){
        command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject', paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs[i], '"', sep = "")
        colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject", additionalMetadataTags)
      } else {
        command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "',dirs[i], '"', sep = "")
        colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject")
      }

    } else {

      if(hasArg(additionalMetadataTags)){
        command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal',paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs[i], '"', sep = "")
        colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", additionalMetadataTags)
      } else {
        command.tmp <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
        colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")
      }
    }

    tmp1 <-  strsplit(system(command.tmp, intern=TRUE), split = "\t")

    # build matrix for image metadata (station i)
    metadata.tmp <- as.data.frame(matrix(unlist(lapply(tmp1, FUN = function(X){X[2]})),
                                         ncol = length(colnames.tmp),
                                         byrow = TRUE),
                                  stringsAsFactors = FALSE)

    colnames(metadata.tmp) <- colnames.tmp

    # now split HierarchicalSubject tags and add as columns to table
    if(customMetadataTags == TRUE){

      metadata.tagname <- "HierarchicalSubject"

      metadata.tmp[,metadata.tagname] <- as.character(metadata.tmp[,metadata.tagname])
      tmp2 <- strsplit(metadata.tmp[,metadata.tagname], split = ",")      # split items of "Subject" at comma
      tmp3 <- lapply(tmp2, FUN = function(X){X[grep(pattern = metadataHierarchyDelimitor, x = X, fixed = TRUE)]})   # get only the ones with values


      # get all unique metadata categories and delete spaces in tag names
      list.tmp <- vector()
      for(xy in 1:length(tmp3)){
        list.tmp <- c(list.tmp, gsub(pattern = " ", replacement = "",
                                     x =  unlist(lapply(strsplit(tmp3[[xy]], split = metadataHierarchyDelimitor, fixed = TRUE), FUN = function(Y){Y = Y[1]}))))
      }
      cols2add <- unique(list.tmp)    # these are the columns to add

      if(length(cols2add) >= 1){
        metadata.tmp <- data.frame(metadata.tmp, matrix(NA, ncol = length(cols2add), nrow = nrow(metadata.tmp)))
        colnames(metadata.tmp)[seq((ncol(metadata.tmp) - length(cols2add) + 1),ncol(metadata.tmp))] <- cols2add

        # add metadata as columns
        for(xyz in 1:length(cols2add)){
          metadata.tmp[,cols2add[xyz]] <- unlist(lapply(lapply(tmp3, FUN = function(X) {sapply(strsplit(X[grep(x = X, pattern = paste(cols2add[xyz], metadataHierarchyDelimitor, collapse = "", sep = ""), fixed = TRUE)], split = metadataHierarchyDelimitor, fixed = TRUE),
                                                                                               FUN = function(Y){Y[2]})}), FUN = function(Z){paste(Z, collapse = ",") }))
          metadata.tmp[which(metadata.tmp[,cols2add[xyz]] == ""), cols2add[xyz]] <- NA

        }
      }
      which_cols_to_rename <- which(colnames(metadata.tmp) %in% cols2add)
      rm(tmp2, tmp3)
    }

    colnames(metadata.tmp) <- gsub(pattern = "[[:blank:]]", replacement = "", x = colnames(metadata.tmp))
    colnames(metadata.tmp) <- gsub(pattern = "[[:punct:]]", replacement = "", x = colnames(metadata.tmp))

    if(customMetadataTags == TRUE){
      colnames(metadata.tmp)[which_cols_to_rename] <- paste("metadata_", colnames(metadata.tmp)[which_cols_to_rename], sep = "")
      rm(cols2add)
    }

    rm(colnames.tmp, tmp1)

    if(length(metadata.tmp) == 0){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      print(paste(dirs[i], "seems to contain no images;", " found", length.tmp, "JPEGs"))

    } else {

      print(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))


      # add species names to metadata table (from folders or metadata, otherwise NA)
      if(hasSpeciesFolders == TRUE){
        metadata.tmp[,speciesCol] <-  sapply(strsplit(metadata.tmp$Directory, split = "/", fixed = TRUE), FUN = function(X){X[length(X)]})
      } else {
        if(customMetadataTags == TRUE & hasArg(metadataSpeciesTag)){
          metadataSpeciesTag2 <- paste("metadata_", metadataSpeciesTag, sep = "")
          if(metadataSpeciesTag2 %in% colnames(metadata.tmp)){

            metadata.tmp[,speciesCol] <- metadata.tmp[,metadataSpeciesTag2]
            metadata.tmp.nrow <- nrow(metadata.tmp)
            species_records_to_remove <- which(is.na(metadata.tmp[,speciesCol]))
            if(length(species_records_to_remove) >= 1){
              metadata.tmp <- metadata.tmp[-species_records_to_remove,]      #remove records without species tag
              warning(paste( dirs_short[i],":  removed", length(species_records_to_remove), "records out of", metadata.tmp.nrow,
                             "because of missing species metadata tag"), call. = FALSE)
            }
            rm(species_records_to_remove, metadata.tmp.nrow)
          } else {
            stop(paste("station", dirs_short[i], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata.", sep = ""))
          }
        } else {
          stop(paste("station", dirs_short[i], ":   cannot figure out species names (metadataSpeciesTag not specified or customMetadataTags == FALSE)"))
        }
      }


      # add station and camera id to metadata table
      if(hasArg(cameraID)){
        if(cameraID == "filename"){
          metadata.tmp <- cbind(metadata.tmp,
                                dirs_short[i],
                                sapply(strsplit(as.character(metadata.tmp$FileName), split = "__"), FUN = function(X){X[2]})      # assumes filenames: Station__Camera__Date/Time(Number).JPG
          )
        } else {
          if(cameraID == "directory"){
            metadata.tmp <- cbind(metadata.tmp,
                                  dirs_short[i],
                                  sapply(strsplit(metadata.tmp$Directory, split = "/", fixed = TRUE), FUN = function(X){X[length(X) - 1]})  # assumes directory structure: Station/Camera/Species
            )
          }
        }
        colnames(metadata.tmp)[ncol(metadata.tmp) - 1] <- stationCol
        colnames(metadata.tmp)[ncol(metadata.tmp)] <- cameraCol
      } else {
        metadata.tmp <- cbind(metadata.tmp,
                              dirs_short[i])
        colnames(metadata.tmp)[ncol(metadata.tmp)] <- stationCol
      }

      # remove species in argument "excluded"
      if(hasArg (exclude)){
        if(length(which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude) == TRUE)) > 0) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }
      }

      # convert character vector extracted from images to time object and format for outfilename
      metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

      if(nrow(metadata.tmp) >= 1){   # if anything left, do

        # prepare to add time difference between observations columns
        metadata.tmp2 <- data.frame(metadata.tmp,
                                    delta.time.secs = NA,
                                    delta.time.mins = NA,
                                    delta.time.hours = NA,
                                    delta.time.days = NA)

        for(xy in 1:nrow(metadata.tmp2)){
          # time difference to all other records of same species at this station (at all cameras)
          if(camerasIndependent == TRUE){
            diff_tmp <- na.omit(difftime(time1 = metadata.tmp2$DateTimeOriginal[xy],
                                         time2 = metadata.tmp2$DateTimeOriginal[metadata.tmp2[,speciesCol] == metadata.tmp2[xy,speciesCol] & 
                                                                                metadata.tmp2[,cameraCol]  == metadata.tmp2[xy,cameraCol] &
                                                                                metadata.tmp2[,stationCol] == metadata.tmp2[xy,stationCol]
                                                                                ],
                                         units = "secs"))
          } else {
            diff_tmp <- na.omit(difftime(time1 = metadata.tmp2$DateTimeOriginal[xy], 
                                         time2 = metadata.tmp2$DateTimeOriginal[metadata.tmp2[,speciesCol] == metadata.tmp2[xy,speciesCol] &
                                                                                metadata.tmp2[,stationCol] == metadata.tmp2[xy,stationCol]
                                                                                ],
                                       units = "secs"))
          }

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

      # append table of station i's images metadata to global record table

        # add potential new columns to record.table

            if(i != 1){
          which_cols_to_add_to_d1 <- seq(1, ncol(record.table))[-which(colnames(record.table) %in% colnames(d1))]   # columns in record.table but not in d1
          
          # if d1 lacks columns present in record.table, add them here (filled with NA)
          if(length(which_cols_to_add_to_d1) >= 1){
            d1 <- data.frame(d1, as.list(rep(NA, each = length(which_cols_to_add_to_d1))))
            colnames(d1)[(ncol(d1) - length(which_cols_to_add_to_d1) + 1) :  ncol(d1)] <- colnames(record.table)[which_cols_to_add_to_d1]
          }
          
          # now check which columns are present in d1 but not in record.table (new tag groups) and add these (filled with NA)
          which_cols_to_add_to_record.table <- seq(1, ncol(d1))[-which(colnames(d1) %in% colnames(record.table))]  # columns present in d1 but not in record.table          
          if(length(which_cols_to_add_to_record.table) >= 1){
            record.table <- data.frame(record.table, as.list(rep(NA, each = length(which_cols_to_add_to_record.table))))
            colnames(record.table)[(ncol(record.table) - length(which_cols_to_add_to_record.table) + 1) :  ncol(record.table)] <- colnames(d1)[which_cols_to_add_to_record.table]
          }
          d2 <- d1[,match(colnames(record.table), colnames(d1))]
          rm(which_cols_to_add_to_d1, which_cols_to_add_to_record.table)
        } else {
          d2 <- d1
        }

        record.table <- rbind(record.table, d2)

        suppressWarnings(rm(d1, d2))
      }
    }
  }

  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Maybe you excluded too many species?"))
  }

  # rearrange table, add date and time as separate columns. add additional column names as needed.

  record.table2  <-  data.frame(record.table[,c(stationCol,
                                                speciesCol, "DateTimeOriginal")],
                                Date = as.Date(record.table$DateTimeOriginal, tz = timeZone, format = "%Y/%M/%d"),
                                Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S",tz = timeZone),
                                record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
                                                "Directory", "FileName")])

  metadata_columns <- which(colnames(record.table) %in% colnames(record.table2) == FALSE)

  if(length(metadata_columns) >= 1){
    record.table3 <- cbind(record.table2, record.table[,metadata_columns])
    colnames(record.table3)[(ncol(record.table2) + 1) : ncol(record.table3)] <- colnames(record.table)[metadata_columns]
  } else {record.table3 <- record.table2}



  if(hasArg(cameraID)){
    record.table3 <- data.frame(record.table3[,stationCol],
                                record.table[,cameraCol],
                                record.table3[,-which(colnames(record.table3) %in% c(stationCol, cameraCol))])
    colnames(record.table3)[1] <- stationCol
    colnames(record.table3)[2] <- cameraCol
  }

  record.table3 <- record.table3[with(record.table3, order(record.table3[,stationCol], record.table3[,speciesCol], DateTimeOriginal)), ]
  rownames(record.table3) <- NULL

  # compute delta time in hours and days
  record.table3$delta.time.secs <- round(record.table3$delta.time.secs, digits = 0)
  record.table3$delta.time.mins <- round(record.table3$delta.time.secs / 60, digits = 0)
  record.table3$delta.time.hours <- round(record.table3$delta.time.mins/60, digits = 1)
  record.table3$delta.time.days <- round(record.table3$delta.time.mins/60/24, digits = 1)

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_database_", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
      write.csv(record.table3, file = outtable_filename)
    } else {

      setwd(outDir)
      write.csv(record.table3, file = outtable_filename)
    }
  }
  return(record.table3)
}