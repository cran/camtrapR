createStationFolders <- function(inDir,
                                 stations,
                                 cameras
                                 ){

# check input
 if(file.exists(inDir) == FALSE) stop("inDir does not exist")
  stopifnot(is.character(stations))
  if(hasArg(cameras)){
  stopifnot(is.character(cameras))
  stopifnot(length(stations) == length(cameras))
  }
 
 # create directories
   if(hasArg(cameras)){
  dirs.to.create <- paste(inDir, stations, cameras, sep = "/")
  
  tmp.create <- suppressWarnings(sapply(dirs.to.create, FUN = dir.create, showWarnings = TRUE, recursive = TRUE))
    dat.out1 <- data.frame(station = stations, 
                           camera = cameras,
                           directory = dirs.to.create,
                           created = tmp.create,
                           exists = file.exists(dirs.to.create))
    rownames(dat.out1) <- NULL

    print(paste("created", sum(tmp.create == TRUE), "directories"))
    if(sum(tmp.create == FALSE) != 0){
      print(paste(sum(tmp.create == FALSE & file.exists(dirs.to.create)), "directories already existed"))
    }
    return(dat.out1)

 } else {
 
 if(any(duplicated(stations))) stop("duplicates in stations are not allowed if cameras is not defined")
  dirs.to.create <- paste(inDir, stations, sep = "/")
  
  
    tmp.create <- suppressWarnings(sapply(dirs.to.create, FUN = dir.create, showWarnings = TRUE, recursive = TRUE))
    dat.out2 <- data.frame(station = stations, 
                            directory = dirs.to.create,
                           created = tmp.create,
                           exists = file.exists(dirs.to.create))
    rownames(dat.out2) <- NULL

    print(paste("created", sum(tmp.create == TRUE), "directories"))
    if(sum(tmp.create == FALSE) != 0){
      print(paste(sum(tmp.create == FALSE & file.exists(dirs.to.create)), "directories already existed"))
    }
    return(dat.out2)
}
}