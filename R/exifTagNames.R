#' Show Exif metadata of JPEG images or other image or video formats
#' 
#' The function will return metadata values, metadata tag names and group names
#' of Exif metadata of JPEG images or other formats.
#' 
#' Many digital cameras record information such as ambient temperature or moon
#' phase under maker-specific tag names in Exif metadata of JPEG images. In
#' addition, many technical information are stored in Exif metadata. In order
#' to extract those information from images and add them to the record tables
#' created by the functions \code{\link{recordTable}} and
#' \code{\link{recordTableIndividual}}, the tag names must be known so they can
#' be passed to these functions via the \code{additionalMetadataTags} argument.
#' 
#' By default the function returns both metadata tag names and the metadata
#' group they belong to (via argument \code{returnTagGroup}). This is helpful
#' to unambiguously address specific metadata tags, because different groups
#' can contain tags of identical names, which may cause problems executing the
#' functions \code{\link{recordTable}} and \code{\link{recordTableIndividual}}.
#' The format is "GROUP:tag", e.g. "EXIF:Flash".
#' 
#' @param inDir character. Directory containing camera trap images sorted into
#' station subdirectories (e.g. inDir/StationA/)
#' @param whichSubDir integer or character. Either number or name of
#' subdirectory of \code{inDir} in which to look for an image
#' @param fileName character. A filename, either the file name of an image in
#' \code{inDir} or a full path with file name (in which case \code{inDir} is
#' not needed)
#' @param returnMetadata deprecated and ignored
#' @param returnTagGroup deprecated and ignored
#' 
#' @return A data frame containing three columns: metadata tag group, tag name,
#' and values.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{recordTable}}
#' 
#' @references Phil Harvey's ExifTool \url{https://exiftool.org/} \cr
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' 
#' # specify directory, camtrapR will automatically take first image from first subdirectory
#' exifTagNames(inDir          = wd_images_ID)
#' 
#' # specify subdirectory by name, camtrapR will use first image
#' exifTagNames(inDir          = wd_images_ID,
#'              whichSubDir    = "StationA")
#' 
#' # specifying fileName only (line break due to R package policy)
#' exifTagNames(fileName       = file.path(wd_images_ID, "StationC", "TRA", 
#'                                         "StationC__2009-05-02__00-10-00(1).JPG"))
#'                                         
#' # specify inDir and fileName
#' exifTagNames(inDir          = wd_images_ID,
#'              fileName       = file.path("StationC", "TRA", "StationC__2009-05-02__00-10-00(1).JPG"))
#'              
#' # it also works this way
#' exifTagNames(inDir          = file.path(wd_images_ID, "StationC", "TRA"),
#'              fileName       = "StationC__2009-05-02__00-10-00(1).JPG")
#'              
#' 
#' # with tagged sample images
#' wd_images_ID_tagged <- system.file("pictures/sample_images_indiv_tag", package = "camtrapR")
#' exifTagNames(inDir          = wd_images_ID_tagged)
#' 
#' }
#' 
#' @export exifTagNames
#' 
exifTagNames <- function(inDir, 
                         whichSubDir = 1, 
                         fileName,
                         returnMetadata = "DEPRECATED", 
                         returnTagGroup = "DEPRECATED"){
  
  
  if(returnMetadata != "DEPRECATED") message("Argument returnMetadata is ignored since version 2.0.0")
  if(returnTagGroup != "DEPRECATED") message("Argument returnTagGroup is ignored since version 2.0.0")
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  
  if(hasArg(fileName)){
    
    if(!hasArg(inDir)){
      file.tmp <- fileName
    } else {
      if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
      file.tmp <- file.path(inDir, fileName)
    }
    if(!file.exists(file.tmp)) stop(paste(file.tmp, "not found"))
    
  } else { # if fileName is not defined
    
    if(!hasArg(inDir))     stop("fileName is not defined. Please define 'inDir'", call. = FALSE)
    if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
    
    if(is.numeric(whichSubDir)){
      dir.tmp0 <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)
      if(length(dir.tmp0) == 0) stop("inDir has no subdirectories")
      dir.tmp <- dir.tmp0[whichSubDir]
      if(is.na(dir.tmp)) stop(paste0("Cound not find subdirectory ", whichSubDir, " in inDir: ", inDir))
    }
    if(is.character(whichSubDir)){
      dir.tmp <- file.path(inDir, whichSubDir)
    }
    if(!dir.exists(dir.tmp)) stop(paste("the specified subdirectory does not exist. Check argument 'whichSubDir.'\n", 
                                        dir.tmp), call. = FALSE)
    
    # filename of first image in specified directory
    file.tmp <- list.files(dir.tmp,
                           full.names = TRUE,
                           pattern    = ".JPG$|.jpg$",
                           recursive  = TRUE)[1]
    if(length(file.tmp) == 0) stop(paste("found no jpg in ", dir.tmp, sep = "\n"))
    
  }
  
  message(paste("Metadata of:", file.tmp, sep = "\n"))

  # generate exiftool calls
  
  # exiftool arguments: 
  # -s1: print tag  names instead of description
  # -t:  tab-delimited output
  # -G: return Group names # 0 (general location), 1 (specific location), 2 (category), 3 (document number), 4 (instance number), 5 (metadata path) or 6 (EXIF/TIFF format)
  
  groupHeadings_general  <- "-G0"    # can possibly also be -G0:1 
  #groupHeadings_specific <- "-G1"   # when using this separately from -G0, output is sorted differently (by group)
  
  
  command.tmp1  <- paste('exiftool -s1 -t ', groupHeadings_general, ' "', file.tmp, '"', sep = "")
  # run without -s1 option to extract tag descriptions instead of tag names
  #command.tmp2 <- paste('exiftool -t ', groupHeadings_general, ' "', file.tmp, '"', sep = "")
  
  # run exiftool
  metadata.tmp1 <- system(command.tmp1, intern=TRUE)
  #metadata.tmp2 <- system(command.tmp2, intern=TRUE)

  # convert output to data frames
  out1 <- read.table(text = metadata.tmp1, header = FALSE, 
                     col.names = c("tag_group", "tag_name", "value"), sep = "\t",
                     stringsAsFactors = FALSE, fill = TRUE)
  # out2 <- read.table(text = metadata.tmp2, header = FALSE, 
  #                    col.names = c("tag_group", "tag_description", "value"), sep = "\t",
  #                    stringsAsFactors = FALSE, fill = TRUE)

  # make sure out1 and out2 are ordered identically
  # if(any(out1$value != out2$value))  stop("Order of exiftool-extracted metadata differs. Please report this bug.")
  # WHY DOES THIS FAIL ON CRAN (debian & fedora, but not on solaris/windows?)
  
  # combine data frames
  out <- cbind(tag_group       = out1$tag_group,
               tag_name        = out1$tag_name,
               # tag_description = out2$tag_description,
               value           = out1$value)
  return(as.data.frame(out))
}
