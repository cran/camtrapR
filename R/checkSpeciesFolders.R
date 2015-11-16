checkSpeciesFolders <- function(inDir,
                                hasCameraSubfolders,
                                maxDeltaTime,
                                excludeSpecies,
                                stationsToCheck,
                                writecsv = FALSE
)
{

  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")
  if(hasArg(excludeSpecies)){
    if(class(excludeSpecies) != "character") stop("excludeSpecies must be of class 'character'")
  }
  if(hasArg(stationsToCheck)){
    if(class(stationsToCheck) != "character") stop("stationsToCheck must be of class 'character'")
  }
  stopifnot(is.logical(hasCameraSubfolders))
  
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  check_table <-  data.frame(stringsAsFactors = FALSE)

  if(hasArg(stationsToCheck)){
    whichStationToCheck <- which(dirs_short %in% stationsToCheck)
    if(length(whichStationToCheck) == 0) {stop("found no directories of names specified in stationsToCheck")}
    dirs <- dirs[whichStationToCheck]
    dirs_short <- dirs_short[whichStationToCheck]
  }

  for(i in 1:length(dirs)){
    # create command line for exiftool execution
    command.tmp <- paste('exiftool  -csv -q -r -f -Directory -FileName -DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
    metadata_tmp <- data.frame(do.call("rbind", strsplit(system(command.tmp, intern=TRUE), split = ",")),
                               stringsAsFactors=FALSE)
    colnames(metadata_tmp) <- as.character(metadata_tmp[1,])
    metadata_tmp <- metadata_tmp[-1,]

    if(class(metadata_tmp) == "data.frame"){

      print(paste(dirs_short[i], ":", nrow(metadata_tmp), "images"))

      dirlist.i <- list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)      # list directories in station directory
      if(hasCameraSubfolders == FALSE){
        filenames.by.folder <- lapply(dirlist.i,
                                      FUN = list.files,
                                      pattern = ".jpg$|.JPG$",
                                      recursive = TRUE,
                                      ignore.case = TRUE)
        names(filenames.by.folder) <- dirlist.i
      } else {
        dirlist.k <- list.dirs(dirlist.i, full.names =TRUE, recursive = FALSE)

        filenames.by.folder <- lapply(dirlist.k,
                                      FUN = list.files,
                                      pattern = ".jpg$|.JPG$",
                                      recursive = TRUE,
                                      ignore.case = TRUE)
        names(filenames.by.folder) <- dirlist.k
        rm(dirlist.k)
      } # end else
      rm(dirlist.i)

      metadata_tmp$species <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                                         split = "/",
                                                         fixed = TRUE),
                                                FUN = function(X){X[length(X)]})),
                                  times = lapply(filenames.by.folder, length))

      if(hasCameraSubfolders == TRUE){
        metadata_tmp$camera <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                                          split = "/",
                                                          fixed = TRUE),
                                                 FUN = function(X){X[length(X) - 1]})),
                                   times = lapply(filenames.by.folder, length))
      }

      if(hasArg (excludeSpecies)){
        if(length(which(tolower(metadata_tmp$species) %in% tolower(excludeSpecies))) > 0) {  # if there is anything to remove
          metadata_tmp <- metadata_tmp[-which(tolower(metadata_tmp$species) %in% tolower(excludeSpecies)),]
        }
      }

      metadata_tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata_tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"))

      metadata_tmp <- cbind(station = rep(dirs_short[i], times = nrow(metadata_tmp)),
                            metadata_tmp)

      # calculate minimum delta time between image and all images in other species folders at station i
      if(length(unique(metadata_tmp$species)) >= 2){

        for(rowindex in 1:nrow(metadata_tmp)){

          if(hasCameraSubfolders){
            # only compare within a camera folder if there was >1 camera per station
            which.tmp1 <- which(metadata_tmp$species != metadata_tmp$species[rowindex] &
                                  metadata_tmp$camera == metadata_tmp$camera[rowindex])
            if(length(which.tmp1) >= 1){
              metadata_tmp$min.delta.time[rowindex] <- round(min(abs(difftime(time1 = metadata_tmp$DateTimeOriginal[rowindex],
                                                                              time2 = metadata_tmp$DateTimeOriginal[which.tmp1],
                                                                              units = "secs"))))
            } else {
              metadata_tmp$min.delta.time[rowindex] <- NA
            }
            rm(which.tmp1)
          } else {
            # compare to other species
            which.tmp2 <- which(metadata_tmp$species != metadata_tmp$species[rowindex])
            if(length(which.tmp2) >= 1){
              metadata_tmp$min.delta.time[rowindex] <- round(min(abs(difftime(time1 = metadata_tmp$DateTimeOriginal[rowindex],
                                                                              time2 = metadata_tmp$DateTimeOriginal[which.tmp2],
                                                                              units = "secs"))))
            } else {
              metadata_tmp$min.delta.time[rowindex] <- NA
            }
            rm(which.tmp2)
          } # end ifelse hasCameraSubfolders
        }  # end for

        if(hasCameraSubfolders == TRUE){
          check_table_tmp <- metadata_tmp[metadata_tmp$min.delta.time <= maxDeltaTime & !is.na(metadata_tmp$min.delta.time), c("station", "Directory", "FileName", "species", "DateTimeOriginal", "camera")]
        } else {
          check_table_tmp <-  metadata_tmp[metadata_tmp$min.delta.time <= maxDeltaTime & !is.na(metadata_tmp$min.delta.time), c("station", "Directory", "FileName", "species", "DateTimeOriginal")]
        }

        check_table_tmp <- check_table_tmp[order(check_table_tmp$DateTimeOriginal),]
        if(ncol(check_table_tmp) >= 1){
          check_table <- rbind(check_table, check_table_tmp)
        }
        suppressWarnings(rm(metadata_tmp, check_table_tmp))

      } # end  if(length(unique(metadata_tmp$species)) >= 2){
    } # end if(class(metadata_tmp) == "data.frame"){
  } # end for (i ...)
  if(writecsv == TRUE){
    check_table_filename <- paste("species_folder_check_", Sys.Date(), ".csv", sep = "")
    setwd(inDir)
    write.csv(check_table, file = check_table_filename)
  }
  return(check_table)
}