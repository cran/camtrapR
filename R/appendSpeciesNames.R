appendSpeciesNames <- function(inDir,
                               hasCameraSubfolders,
                               removeNames = FALSE,
                               writecsv = FALSE
)
{
  stopifnot(is.logical(removeNames))
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasCameraSubfolders))

  dirs <- list.dirs(inDir, recursive = FALSE)

  renaming.table <- data.frame()
  # loop over all subfolders of  main directory
  for(i in 1:length(dirs)){

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
    }

    rm(dirlist.i)
    #######

    folders.tmp <- rep(names(filenames.by.folder),
                       times = lapply(filenames.by.folder,
                                      length))
    species.tmp <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                              split = "/",
                                              fixed = TRUE),
                                     FUN = function(X){X[length(X)]})),
                       times = lapply(filenames.by.folder, length))

    if(hasCameraSubfolders == TRUE){
      camera.tmp <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                               split = "/",
                                               fixed = TRUE),
                                      FUN = function(X){X[length(X) - 1]})),
                        times = lapply(filenames.by.folder, length))
    }

    if(hasCameraSubfolders == FALSE){
      renaming.table <- rbind(renaming.table, data.frame(directory = folders.tmp,
                                                         filename_old = unlist(filenames.by.folder),
                                                         species = species.tmp))
    } else {
      renaming.table <- rbind(renaming.table, data.frame(directory = folders.tmp,
                                                         filename_old = unlist(filenames.by.folder),
                                                         species = species.tmp,
                                                         camera = camera.tmp))
    }
    rownames(renaming.table) <- seq(1,nrow(renaming.table), by = 1)
  }

  # construct new filename
  if(removeNames == FALSE){
    renaming.table$filename_new <- paste(paste(gsub(pattern = ".jpg|.JPG$",
                                                    replacement = "",
                                                    x = renaming.table$filename_old,
                                                    ignore.case = TRUE),
                                               renaming.table$species,
                                               sep = "__"),
                                         ".JPG", sep = "")
  }
  if(removeNames == TRUE){
    renaming.table$filename_new <- gsub(pattern = paste(paste("__",
                                                              unique(renaming.table$species),
                                                              sep = ""),
                                                        collapse = "|"),
                                        replacement = "",
                                        x = renaming.table$filename_old,
                                        ignore.case = TRUE)
  }

  # rename
  renaming.table$renaming_ok <- file.rename(from = paste(renaming.table$directory, renaming.table$filename_old, sep = "/"),
                                            to = paste(renaming.table$directory, renaming.table$filename_new, sep = "/"))
  # write outtable
  if(writecsv == TRUE){
    if(removeNames == TRUE){
      filename.tmp <- paste("renaming_table_UNDO_", Sys.Date(), ".csv", sep = "")
    }
    if(removeNames == FALSE){
      filename.tmp <- paste("renaming_table_", Sys.Date(), ".csv", sep = "")
    }
    setwd(inDir)
    write.csv(renaming.table, file = filename.tmp, row.names = FALSE)
  }

  print(paste("renamed", sum(renaming.table$renaming_ok), "out of", nrow(renaming.table), "images in", inDir))
  return(renaming.table)

}

