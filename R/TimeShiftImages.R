timeShiftImages <- function(inDir,
                            timeShiftTable,
                            stationCol,
                            cameraCol,
                            hasCameraSubfolders,
                            timeShiftColumn,
                            timeShiftSignColumn,
                            undo = FALSE
)
{
  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")

  timeShiftTable <- apply(timeShiftTable, MARGIN = 2, FUN = as.character)

  stopifnot(c(stationCol, timeShiftColumn, timeShiftSignColumn) %in% colnames(timeShiftTable))
  if(isTRUE(hasCameraSubfolders)){
    stopifnot(cameraCol %in% colnames(timeShiftTable))
  }

  stopifnot(is.logical(hasCameraSubfolders))
  for(xy in 1:nrow(timeShiftTable)){
    if(length(unlist(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "))) != 2) stop(paste("there is more than 1 space in your timeShiftColumn string. Only 1 space is allowed (", timeShiftTable[xy,stationCol], ")"))
    if(nchar(timeShiftTable[xy,timeShiftColumn]) - nchar(gsub(":","",timeShiftTable[xy,timeShiftColumn])) != 4) stop("there should be 4 colons in timeShiftColumn (", timeShiftTable[xy,stationCol], ")")
    if(timeShiftTable[xy,timeShiftSignColumn] %in% c("+", "-") == FALSE) stop("timeShiftSignColumn can only be + or - (",
                                                                              timeShiftTable[xy,stationCol], ")")
    if(length(unlist(lapply(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "), FUN = strsplit, split = ":"))) != 6) stop("there must be six numbers in timeShiftColumn (",
                                                                                                                                 timeShiftTable[xy,stationCol], ")")
  }

  if(isTRUE(hasCameraSubfolders)){
    shift.dirs <- paste(inDir, timeShiftTable[,stationCol], timeShiftTable[,cameraCol], sep = "/")
  } else {
    shift.dirs <- paste(inDir, timeShiftTable[,stationCol], sep = "/")
  }

  if(any(file.exists(shift.dirs) == FALSE)){
    stop(paste(shift.dirs[file.exists(shift.dirs) == FALSE], "does not exist"))
  }

  if(isTRUE(undo)){

    for(i in 1:length(shift.dirs)){
      jpg2delete <- list.files(shift.dirs[i], pattern = ".jpg$|.JPG$", recursive = TRUE, full.names = TRUE)
      jpg2keep <- list.files(shift.dirs[i], pattern = ".jpg_original$|.JPG_original$", recursive = TRUE, full.names = TRUE)
      jpg2keep_newname <- gsub(pattern = "_original$", replacement = "", x = jpg2keep)

      if(length(jpg2keep) == 0) stop(paste("found no .JPG_original files in", shift.dirs[i]))
      if(length(jpg2delete) != length(jpg2keep)) stop(paste("number of jpgs in", shift.dirs[i], "is not equal to the number of JPG_original files" ))

      remove.tmp <- file.remove(jpg2delete)
      rename.tmp <- file.rename(from = jpg2keep, to = jpg2keep_newname)

      rm(jpg2delete, jpg2keep, jpg2keep_newname)
    }
  } else {

    results.tmp <- list()

    for(i in 1:nrow(timeShiftTable)){
      print(shift.dirs[i])
      list.files(shift.dirs)
      command.tmp2b <- paste('exiftool -r "-DatetimeOriginal', timeShiftTable[i,timeShiftSignColumn], '=',
                             timeShiftTable[i,timeShiftColumn], '" "', shift.dirs[i], '"', sep = "")
      results.tmp[[i]] <- system(command.tmp2b, intern=TRUE)
      rm(command.tmp2b)
    }

    output <- data.frame(shift.dirs, matrix(unlist(results.tmp, recursive = FALSE),ncol = 2, byrow = 2))
    colnames(output) <- c("directory", "n_directories", "n_images")
    return(output)
  }
}