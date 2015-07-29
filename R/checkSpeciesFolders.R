checkSpeciesFolders <- function(inDir,
                                maxDeltaTime,
                                exclude,
                                writecsv = FALSE
)
{

  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")
  if(hasArg(exclude)){
    if(class(exclude) != "character") stop("exclude must be of class 'chararcter'")
  }
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  check_table <-  data.frame(stringsAsFactors = FALSE)


  for(i in 1:length(dirs)){
    # create command line for exiftool execution
    command.tmp <- paste('exiftool  -csv -q -r -f -Directory -FileName -DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
    metadata_tmp <- data.frame(do.call("rbind", strsplit(system(command.tmp, intern=TRUE), split = ",")),
                               stringsAsFactors=FALSE)
    colnames(metadata_tmp) <- as.character(metadata_tmp[1,])
    metadata_tmp <- metadata_tmp[-1,]

    if(class(metadata_tmp) == "data.frame"){

      print(paste(dirs_short[i], ":", nrow(metadata_tmp), "images"))

      if(length(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)) == 1){
        filenames.by.folder <- list(list.files(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE),
                                               pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE))
        names(filenames.by.folder) <- list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)
      } else {
        filenames.by.folder <- lapply(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE),
                                      FUN = list.files, pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE)
        names(filenames.by.folder) <- list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)
      }

      metadata_tmp$species <-   gsub("/", "", rep(unlist(lapply(strsplit(names(filenames.by.folder), split = dirs[i], fixed = TRUE), FUN = function(X){X[[2]]})),
                                                  times = lapply(filenames.by.folder, length)))

      if(hasArg (exclude)){
        if(length(which(tolower(metadata_tmp$species) %in% tolower(exclude))) > 0) {  # if there is anything to remove
          metadata_tmp <- metadata_tmp[-which(tolower(metadata_tmp$species) %in% tolower(exclude)),]
        }
      }

      metadata_tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata_tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"))
      metadata_tmp <- cbind(station = rep(dirs_short[i], times = nrow(metadata_tmp)),
                            metadata_tmp)

      # calculate minimum delta time between image and all images in other species folders at station i
      if(length(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)) > 1){
        for(rowindex in 1:nrow(metadata_tmp)){
          metadata_tmp$min.delta.time[rowindex] <- round(min(abs(difftime(metadata_tmp$DateTimeOriginal[rowindex],
                                                                          metadata_tmp$DateTimeOriginal[-which(metadata_tmp$Directory == metadata_tmp$Directory[rowindex])],
                                                                          units = "secs"))))
        }
      }
      # if photos of different species within some time interval make sure this is really the case (check images in this table)
      check_table_tmp <-  metadata_tmp[metadata_tmp$min.delta.time <= maxDeltaTime, c("station", "Directory", "FileName", "species", "DateTimeOriginal")]
      check_table_tmp <- check_table_tmp[order(check_table_tmp$DateTimeOriginal),]
      check_table <- rbind(check_table, check_table_tmp)
      suppressWarnings(rm(metadata_tmp, check_table_tmp))
    }
  }
  if(writecsv == TRUE){
    check_table_filename <- paste("species_folder_check_", Sys.Date(), ".csv", sep = "")
    setwd(inDir)
    write.csv(check_table, file = check_table_filename)
  }
  return(check_table)
}