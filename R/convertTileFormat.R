#function to convert map tile directory between OSM and TMS formats

#' Converts existing map tile directory between OSM and TMS specifications
#' 
#' Takes an existing map directory of the form "dir/{z}/{x}/{y}.filetype", 
#' and switches format of directory between OSM and TMS formats. The function
#' cannot check the format of the directory as it's passed to the function, but
#' mistakes can be rectified by a reverse call.
#' 
#' @param dir a character denoting the path to the tile directory.
#' @author TJ McKinley
#' @references http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
#'
#' @export convertTileFormat

convertTileFormat <- function(dir)
{
	#some checks to ensure correct input
	if(missing(dir))
	{
		cat("'dir' argument missing\n")
		return(1)
	}
	if(!is.character(dir))
	{
		cat("'dir' not a character\n")
		return(1)
	}
	if(length(dir) > 1)
	{
		cat("'dir' has length > 1, so only first element is used\n")
		dir <- dir[1]
	}
		
	#now check directory exists
	if(file.info(dir)$isdir == FALSE)
	{
		cat(paste("Can't access", dir, "directory\n"))
		return(1)
	}
		
	#given that directory exists, extract all zoom levels
	zooms <- list.files(dir)
	zooms <- as.numeric(zooms)
	if(min(zooms) < 0 | max(zooms) > 18)
	{
		cat("Zoom levels in directory invalid\n")
		return(1)
	}
	
	#now cycle through zoom levels and rename all images accordingly
	for(i in zooms)
	{
		subdir <- file.path(dir, i)
		#extract x-coordinates 
		#(CURRENTLY NO CHECK THAT THESE ARE VALID FOR THE GIVEN ZOOM LEVEL)
		xcoords <- list.files(subdir)
		xcoords <- as.numeric(xcoords)
		
		nrows <- 2 ^ i
		
		#cycle through x-coordinates
		for(j in xcoords)
		{
			subdir1 <- file.path(subdir, j)
			#extract all filenames
			ycoords <- list.files(subdir1)
			#extract filetype of images
			filetype <- lapply(as.list(ycoords), function(x) strsplit(x, "\\.")[[1]])
			filetype <- sapply(filetype, function(x) x[length(x)])
			if(!all(filetype == filetype[1]))
			{
				cat(paste("Filetypes don't match in", subdir1, "directory\n"))
				return(1)
			}
			filetype <- filetype[1]
			
			#extract names of files
			ycoords <- sapply(as.list(ycoords), function(x, filetype) strsplit(x, paste0(".", filetype))[[1]], filetype = filetype)
			ycoords <- as.numeric(ycoords)
			
			for(k in ycoords)
			{
				#rename files accordingly (creating a dummy name first to avoid duplication
				#of filenames)
				file.rename(from = file.path(subdir1, paste0(k, ".", filetype)), to = file.path(subdir1, paste0(k, "blah.", filetype)))
			}
			for(k in ycoords)
			{
				#rename dummy files accordingly 
				file.rename(from = file.path(subdir1, paste0(k, "blah.", filetype)), to = file.path(subdir1, paste0(nrows - k - 1, ".", filetype)))
			}
		}
	}
	cat("Directory converted successfully\n")
}
	
	
	
	
	
	
