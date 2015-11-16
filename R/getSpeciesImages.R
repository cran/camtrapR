getSpeciesImages <- function(species,
                             inDir,
                             outDir,
                             stationSubfolders = FALSE)
  {

  stopifnot(is.logical(stationSubfolders))
  if (isTRUE(all(unlist(strsplit(tolower(inDir), split = "/")) %in%
                 unlist(strsplit(tolower(outDir), split = "/")))))
    stop("outDir may not be identical to or a subdirectory of inDir")

  if (length(list.files(paste(outDir, species, sep = "/"),
                        recursive = TRUE, full.names = TRUE, pattern = ".jpg$|.JPG$")) !=   0){
    stop(paste(outDir, "/", species, " is not empty",  sep = ""))
}

  copy.table <- data.frame()
  dir.list <- list.dirs(inDir, recursive = TRUE, full.names = TRUE)
  dir.list.short <- list.dirs(inDir, recursive = TRUE, full.names = FALSE)
  spec.index <- grep(species, dir.list, ignore.case = TRUE)
  if (length(spec.index) == 0)
    stop(paste("found no folder named  '", species, "' in ",
               inDir, sep = ""))
  dir.list2 <- dir.list[spec.index]
  dir.list.short2 <- dir.list.short[spec.index]
  if (isTRUE(stationSubfolders)) {
    dirs.out <- paste(outDir, species, sapply(dir.list.short2,
                                              FUN = function(X) {
                                                unlist(strsplit(X, split = "/"))[1]
                                              }), sep = "/")
    for (i in 1:length(dirs.out)) {
      files.tmp <- list.files(dir.list2[i], recursive = FALSE,
                              full.names = TRUE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")
      files.tmp.short <- list.files(dir.list2[i], recursive = FALSE,
                                    full.names = FALSE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")
      copy.table.tmp <- as.data.frame(matrix(nrow = length(files.tmp),
                                             ncol = 3))
      colnames(copy.table.tmp) <- c("imageFrom", "imageTo",
                                    "copy_ok")
      dir.create(path = dirs.out[i], recursive = TRUE,
                 showWarnings = FALSE)
      copy.table.tmp$imageFrom <- files.tmp
      
      duplicate.check <- sapply(strsplit(copy.table.tmp$imageFrom, split = "/", fixed = TRUE), FUN = function(X){X[length(X)]})
      if(any(duplicated(duplicate.check))){
        print(table(duplicate.check)[table(duplicate.check) >= 2])
        stop("there are duplicate filenames", call. = FALSE)
      }
      
      copy.table.tmp$copy_ok <- file.copy(from = files.tmp,
                                          to = dirs.out[i])
      copy.table.tmp$imageTo <- paste(dirs.out[i], files.tmp.short,
                                      sep = "/")
      copy.table <- rbind(copy.table, copy.table.tmp)
      rm(copy.table.tmp)
    }
    print(paste(toupper(species), " - copied", sum(copy.table$copy_ok),
                "out of", nrow(copy.table), "images in", inDir, "to",
                length(unique(dirs.out)), "subfolders of", paste(outDir,
                                                                 species, sep = "/"), sep = " "))
  }
  else {
    dir.out <- paste(outDir, species, sep = "/")
    files.tmp <- list.files(dir.list2, recursive = FALSE,
                            full.names = TRUE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")
    files.tmp.short <- list.files(dir.list2, recursive = FALSE,
                                  full.names = FALSE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")
    copy.table <- as.data.frame(matrix(nrow = length(files.tmp),
                                       ncol = 3))
    colnames(copy.table) <- c("imageFrom", "imageTo", "copy_ok")
    dir.create(path = dir.out, recursive = TRUE, showWarnings = FALSE)
    copy.table$imageFrom <- files.tmp

    duplicate.check <- sapply(strsplit(copy.table$imageFrom, split = "/", fixed = TRUE), FUN = function(X){X[length(X)]})
      if(any(duplicated(duplicate.check))){
        print(table(duplicate.check)[table(duplicate.check) >= 2])
        stop("there are duplicate filenames", call. = FALSE)
      }
    copy.table$copy_ok <- file.copy(from = files.tmp, to = dir.out)
    copy.table$imageTo <- paste(dir.out, files.tmp.short,
                                sep = "/")

    print(paste(toupper(species), " - copied", sum(copy.table$copy_ok),
                "out of", nrow(copy.table), "images in", inDir, "to",
                dir.out, sep = " "))
  }
  copy.table <- cbind(species = species, copy.table)
  return(copy.table)
}