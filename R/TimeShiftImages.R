TimeShiftImages <- function(inDir,
                            TimeShiftTable,
                            stationCol,
                            cameraCol,
                            hasCameraSubfolders,
                            TimeShiftColumn,
                            TimeShiftSignColumn,
                            undo = FALSE
)
{
  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")

  TimeShiftTable <- apply(TimeShiftTable, MARGIN = 2, FUN = as.character)

  stopifnot(c(stationCol, TimeShiftColumn, TimeShiftSignColumn) %in% colnames(TimeShiftTable))
  if(isTRUE(hasCameraSubfolders)){
    stopifnot(cameraCol %in% colnames(TimeShiftTable))
  }

  stopifnot(is.logical(hasCameraSubfolders))
  for(xy in 1:nrow(TimeShiftTable)){
    if(length(unlist(strsplit(TimeShiftTable[xy,TimeShiftColumn], split = " "))) != 2) stop(paste("there is more than 1 space in your TimeShiftColumn string. Only 1 space is allowed (", TimeShiftTable[xy,stationCol], ")"))
    if(nchar(TimeShiftTable[xy,TimeShiftColumn]) - nchar(gsub(":","",TimeShiftTable[xy,TimeShiftColumn])) != 4) stop("there should be 4 colons in TimeShiftColumn (", TimeShiftTable[xy,stationCol], ")")
    if(TimeShiftTable[xy,TimeShiftSignColumn] %in% c("+", "-") == FALSE) stop("TimeShiftSignColumn can only be + or - (",
                                                                              TimeShiftTable[xy,stationCol], ")")
    if(length(unlist(lapply(strsplit(TimeShiftTable[xy,TimeShiftColumn], split = " "), FUN = strsplit, split = ":"))) != 6) stop("there must be six numbers in TimeShiftColumn (",
                                                                                                                                 TimeShiftTable[xy,stationCol], ")")
  }

  if(isTRUE(hasCameraSubfolders)){
    shift.dirs <- paste(inDir, TimeShiftTable[,stationCol], TimeShiftTable[,cameraCol], sep = "/")
  } else {
    shift.dirs <- paste(inDir, TimeShiftTable[,stationCol], sep = "/")
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

    for(i in 1:nrow(TimeShiftTable)){
      print(shift.dirs[i])
      list.files(shift.dirs)
      command.tmp2b <- paste('exiftool -r "-DateTimeOriginal', TimeShiftTable[i,TimeShiftSignColumn], '=',
                             TimeShiftTable[i,TimeShiftColumn], '" "', shift.dirs[i], '"', sep = "")
      results.tmp[[i]] <- system(command.tmp2b, intern=TRUE)
      rm(command.tmp2b)
    }

    output <- data.frame(shift.dirs, matrix(unlist(results.tmp, recursive = FALSE),ncol = 2, byrow = 2))
    colnames(output) <- c("directory", "n_directories", "n_images")
    return(output)
  }
}