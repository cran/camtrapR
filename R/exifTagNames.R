exifTagNames <- function(inDir, whichSubDir = 1){

  stopifnot(file.exists(inDir))
  if(Sys.which("exiftool") == "") stop("cannot find Exiftool")
  dirs.tmp <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)

  if(file.exists(dirs.tmp[whichSubDir]) == FALSE) stop("the specified subdirectory does not exist")
  
  file.tmp <- list.files(dirs.tmp[whichSubDir],
                         full.names = TRUE,
                         pattern = ".JPG$|.jpg$",
                         recursive = TRUE)[1]
  if(length(file.tmp) == 0) stop(paste("found no jpg in ", dirs.tmp[whichSubDir], sep = "\n"))

  command.tmp <- paste('exiftool -csv "', file.tmp, '"', sep = "")
  metadata.tmp <- system(command.tmp, intern=TRUE)
  tagnames <- sort(unlist(strsplit(metadata.tmp[[1]], split = ",")))

  return (tagnames)
}