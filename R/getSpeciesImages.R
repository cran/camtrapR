#' Collect all images of a species
#' 
#' This function will fetch all images of a particular species from all camera
#' trap stations and copies these images to a new location. The images which
#' are to be copied are found in one of 2 possible ways, 1) by providing an
#' existing record table (created with \code{\link{recordTable}}) or 2) by
#' reading species IDs from species directories or from metadata (calling
#' ExifTool). Earlier in the workflow, i.e., before running this function,
#' images should have been renamed (with \code{\link{imageRename}}) to give
#' images unique file names based on station ID and date/time.
#' 
#' 
#' The function finds the images to be copied by either consulting a record
#' table created with \code{\link{recordTable}} or by reading species IDs from
#' images. The former is considerable faster because ExifTool is not called,
#' but requires images to be in precisely the location given by the columns
#' \code{Directory} and \code{FileName} in \code{recordTable}. To use this
#' feature, provide the function with a record table in argument
#' \code{recordTable}.
#' 
#' If you'd rather read species IDs from images within the function (to make
#' sure all file paths are correct), images need to be in the directory
#' structure required by the package, e.g.
#' 
#' > inDir/Station/Species
#' 
#' or
#' 
#' > inDir/Station/Camera/Species
#' 
#' if using species directories for species IDs, and
#' 
#' > inDir/Station
#' 
#' or
#' 
#' > inDir/Station/Camera
#' 
#' if reading IDs from species metadata tags. In the latter case, only station
#' directories are needed. In any case, the argument \code{species} must match
#' species IDs (either the \code{speciesCol} in \code{recordTable}, species
#' directory names or species metadata tags).
#' 
#' Before running the function, first rename the images using function
#' \code{\link{imageRename}} to provide unique file names and prevent several
#' images from having the same name (if generic names like "IMGP0001.jpg" are
#' used). The function will not copy images if there are duplicate filenames to
#' prevent overwriting images unintentionally.
#' 
#' @param species character. Species whose images are to be fetched
#' @param recordTable data frame. A data frame as returned by function
#' \code{\link{recordTable}}. If you specify this argument, do not specify
#' \code{inDir}
#' @param speciesCol character. Name of the column specifying species ID in
#' \code{recordTable}. Only required if \code{recordTable} is defined
#' @param stationCol character. Name of the column specifying station ID in
#' \code{recordTable}. Only required if \code{recordTable} is defined
#' @param inDir character. Directory containing identified (species level)
#' camera trap images sorted into station subdirectories (e.g.
#' inDir/StationA/). If you specify this argument, do not specify
#' \code{recordTable}.
#' @param outDir character. Directory in which to save species images. A
#' species subdirectory will be created in \code{outDir} automatically.
#' @param createStationSubfolders logical. Save images in station directories
#' within the newly created species directory in \code{outDir}?
#' @param IDfrom character. Read species ID from image metadata ("metadata") of
#' from species directory names ("directory")? Only required if \code{inDir} is
#' defined.
#' @param metadataSpeciesTag character. The species ID tag name in image
#' metadata (if IDfrom = "metadata"). Only required if \code{inDir} is defined.
#' @param metadataHierarchyDelimitor character. The character delimiting
#' hierarchy levels in image metadata tags in field "HierarchicalSubject".
#' Either "|" or ":" (if IDfrom = "metadata"). Only required if \code{inDir} is
#' defined and \code{IDfrom = "metadata"}.
#' 
#' @return A \code{data.frame} with old and new directories and file names and
#' the copy status (\code{copy_ok}; TRUE if copying was successful, FALSE if
#' not).
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' \dontrun{
#' # define image directory
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' wd_images_ID_copy <- file.path(tempdir(), "sample_images_species_dir")
#' 
#' species_to_copy <- "VTA"    # = Viverra tangalunga, Malay Civet
#' 
#' specImagecopy <- getSpeciesImages(species                 = species_to_copy,
#'                                   inDir                   = wd_images_ID,
#'                                   outDir                  = wd_images_ID_copy,
#'                                   createStationSubfolders = FALSE,
#'                                   IDfrom                  = "directory"
#'                                   )
#' }
#' 
#' @export getSpeciesImages
#' 
getSpeciesImages <- function(species,
                             recordTable,
                             speciesCol = "Species",
                             stationCol = "Station",
                             inDir,
                             outDir,
                             createStationSubfolders = FALSE,
                             IDfrom,
                             metadataSpeciesTag,
                             metadataHierarchyDelimitor = "|")
{

  file.sep <- .Platform$file.sep

  if(hasArg(recordTable) & hasArg(inDir)) stop("recordTable and inDir cannot be both defined. Please define one only.", call. = FALSE)
    
  
  stopifnot(is.logical(createStationSubfolders))

  if(hasArg(inDir)){
    if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
    if (isTRUE(all(unlist(strsplit(tolower(inDir), split = file.sep)) %in%
                   unlist(strsplit(tolower(outDir), split = file.sep)))))    stop("outDir may not be identical to or a subdirectory of inDir", call. = FALSE)

    if(!is.character(IDfrom)) stop("IDfrom must be of class 'character'", call. = FALSE)
    if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'", call. = FALSE)

    if(IDfrom == "metadata"){
      stopifnot(metadataHierarchyDelimitor %in% c("|", ":"))
      metadata.tagname <- "HierarchicalSubject"
      if(Sys.which("exiftool") == "")       stop("cannot find ExifTool")
    }
  }

  if(hasArg(recordTable)){
    recordTable <- dataFrameTibbleCheck(df = recordTable)
    checkForSpacesInColumnNames(speciesCol = speciesCol)
  #if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  
  if(!speciesCol %in% colnames(recordTable))  stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)

    if(createStationSubfolders == TRUE &
       stationCol %in% colnames(recordTable) == FALSE)  {
          stop(paste('stationCol = "', stationCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
          checkForSpacesInColumnNames(stationCol = stationCol)
          }
    if(species %in% recordTable[,speciesCol] == FALSE)  stop("species was not found in speciesCol of recordTable", call. = FALSE)
  }

  if (length(list.files(file.path(outDir, species),
                        recursive = TRUE, full.names = TRUE, pattern = ".jpg$|.JPG$")) !=   0){
    stop(paste("target directory ", file.path(outDir, species), " is not empty"), call. = FALSE)
  }


  if(hasArg(metadataSpeciesTag)){
    if(!is.character(metadataSpeciesTag)){stop("metadataSpeciesTag must be of class 'character'", call. = FALSE)}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1", call. = FALSE)}
  }



  multiple_tag_separator = "_&_"

  # define directories to save images to
  dirs.out0 <- file.path(outDir, species)


  if(hasArg(inDir)){

    copy.table <- data.frame()

    if(IDfrom == "directory"){

      dir.list <- list.dirs(inDir, recursive = TRUE, full.names = TRUE)         # recursive to also find species directories in camera subdirectories
      dir.list.short <- list.dirs(inDir, recursive = TRUE, full.names = FALSE)

      # find directories of requested species
      spec.index <- grep(species, dir.list, ignore.case = TRUE)

      if (length(spec.index) == 0)      stop(paste("found no directory named  '", species, "' in ", inDir, sep = ""), call. = FALSE)
      dir.list2 <- dir.list[spec.index]
      dir.list.short2 <- dir.list.short[spec.index]

      # find the station directories that have species records

      for (i in 1:length(dir.list2)) {
        files.tmp <- list.files(dir.list2[i], recursive = FALSE,
                                full.names = TRUE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")
        files.tmp.short <- list.files(dir.list2[i], recursive = FALSE,
                                      full.names = FALSE, include.dirs = FALSE, pattern = ".jpg$|.JPG$")


        copy.table.tmp <- as.data.frame(matrix(nrow = length(files.tmp),
                                               ncol = 5))
        colnames(copy.table.tmp) <- c("DirectoryOrig", "FilenameOrig", "DirectoryCopy",  "FilenameCopy", "copy_ok")

        copy.table.tmp$DirectoryOrig <- dir.list2[i]
        copy.table.tmp$FilenameOrig <- files.tmp.short

        if(isTRUE(createStationSubfolders))  copy.table.tmp$DirectoryCopy <- file.path(dirs.out0, unlist(strsplit(dir.list.short2[i], split = file.sep))[1])
        if(isFALSE(createStationSubfolders)) copy.table.tmp$DirectoryCopy <- dirs.out0
        copy.table.tmp$FilenameCopy <- files.tmp.short

        copy.table <- rbind(copy.table, copy.table.tmp)

        rm(copy.table.tmp)
      }

    } else {

      dirs <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)     # exiftool will work through directory recursively, so set to FALSE here
      dirs_short <- list.dirs(inDir, recursive = FALSE, full.names = FALSE)

      if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)


      for(i in 1:length(dirs_short)){

        command.tmp  <- paste('exiftool -t -q -r -f -Directory -FileName -HierarchicalSubject -ext JPG "', dirs[i], '"', sep = "")
        colnames.tmp <- c("Directory", "FileName", "HierarchicalSubject")

        # run exiftool and make data frame
        metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)


        if(is.data.frame(metadata.tmp)){

          message(paste(dirs_short[i], ": ", formatC(nrow(metadata.tmp), width = 4), " images", 
                        makeProgressbar(current = i, total = length(dirs_short)), sep = ""))

          # add metadata
          metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                                metadata.tagname           = metadata.tagname,
                                                metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                                multiple_tag_separator     = multiple_tag_separator)

          # assign species ID
          metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,
                                           IDfrom                 = IDfrom,
                                           metadataSpeciesTag     = metadataSpeciesTag,
                                           speciesCol             = "species",
                                           dirs_short             = dirs_short,
                                           i_tmp                  = i,
                                           multiple_tag_separator = multiple_tag_separator,
                                           returnFileNamesMissingTags = FALSE
          )

          # filter by species
          metadata.tmp <- metadata.tmp[metadata.tmp$species == species,]

          if(nrow(metadata.tmp) >= 1){
            copy.table.tmp <- data.frame(DirectoryOrig = metadata.tmp$Directory,
                                         FilenameOrig  = metadata.tmp$FileName,
                                         DirectoryCopy = NA,
                                         FilenameCopy  = metadata.tmp$FileName)

            if(isTRUE(createStationSubfolders))  copy.table.tmp$DirectoryCopy <- file.path(dirs.out0, dirs_short[i])
            if(isFALSE(createStationSubfolders)) copy.table.tmp$DirectoryCopy <- dirs.out0

            copy.table <- rbind(copy.table, copy.table.tmp)
          }

        } # end if(is.data.frame(metadata.tmp)){
      }   # end for(i in 1:length(dirs_short)){
    }     # end else (IDfrom)

    # if species not found, exit
    if(nrow(copy.table) == 0) {stop(paste("did not find species '", species, "' in  ", inDir, sep = ""), call. = FALSE)}

  }       # end if(hasArg(inDir))

  if(hasArg(recordTable)){

    # species subset from record Table
    species_subset <- recordTable[recordTable[,speciesCol] == species,]

    if(isTRUE(createStationSubfolders))  DirectoryCopy.tmp <- file.path(dirs.out0, species_subset[,stationCol])
    if(isFALSE(createStationSubfolders)) DirectoryCopy.tmp <- dirs.out0

    # fill copy.table
    copy.table <- data.frame(DirectoryOrig = species_subset$Directory,
                             FilenameOrig  = species_subset$FileName,
                             DirectoryCopy = DirectoryCopy.tmp,
                             FilenameCopy  = species_subset$FileName,
                             stringsAsFactors = FALSE)

    missing_files <- !file.exists(file.path(copy.table$DirectoryOrig, copy.table$FilenameOrig))

    if(any(missing_files)) stop("Did not find images from recordTable, e.g.: \n",
                                paste(head(
                                  file.path(copy.table[missing_files, 1],
                                            copy.table[missing_files, 2])
                                ), collapse = "\n"),
                                call. = FALSE)

  }

  # check if there are any duplicate file names (would cause overwriting of images)
  if(any(tapply(copy.table$FilenameCopy,
                INDEX = copy.table$DirectoryCopy,
                FUN   = function(X){any(duplicated(X))}))){
    message(table(copy.table$FilenameCopy)[table(copy.table$FilenameCopy) >= 2])
    stop("Cannot copy. there are duplicate filenames", call. = FALSE)
  }

  # create directories
  sapply(unique(copy.table$DirectoryCopy), FUN = dir.create, recursive = TRUE, showWarnings = FALSE)

  # copy images
  copy.table$copy_ok <- file.copy(from = file.path(copy.table$DirectoryOrig, copy.table$FilenameOrig),
                                  to   = file.path(copy.table$DirectoryCopy, copy.table$FilenameCopy))

  # little summary line
  if(hasArg(inDir)){
    message(paste(toupper(species), " - copied", sum(copy.table$copy_ok),
                  "out of", nrow(copy.table), "images in", inDir, "  to  ",
                  file.path(outDir, species)))
  }
  if(hasArg(recordTable)){
    message(paste(toupper(species), " - copied", sum(copy.table$copy_ok),
                  "out of", nrow(copy.table), "images to  ",
                  file.path(outDir, species)))
  }

  copy.table <- cbind(species = species, copy.table)
  return(copy.table)
}
