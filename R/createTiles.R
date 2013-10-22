#function that takes a SpatialPolygonsDataFrame object and produces map tiles

createTiles <- function(object, title, max_zoom = 18, tms = FALSE, ask = TRUE)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(missing(title)) stop("'title' missing\n")
	if(missing(max_zoom)) stop("'max_zoom' missing\n")
	if(class(object) != "SpatialPolygonsTiles") stop("'object' not a 'SpatialPolygonsTiles' object\n")
	if(!is.character(title[1])) stop("'title' is not a character\n")
	if(length(title) > 1)
	{
		cat("'title' has length > 1, so only first element is used\n")
		title <- title[1]
	}
	if(!is.numeric(max_zoom[1])) stop("'max_zoom' is not numeric\n")
	if(length(max_zoom) > 1)
	{
		cat("'max_zoom' has length > 1, so only first element is used\n")
		max_zoom <- max_zoom[1]
	}
	if(round(max_zoom) != max_zoom) stop("'max_zoom' is not an integer\n")
	if(max_zoom < 0 | max_zoom > 18) stop("'max_zoom' not in the range 0-18\n")
	if(!is.logical(tms)) stop("'tms' is not a logical value")
	if(length(tms) > 1)
	{
		cat("'tms' has length > 1, so only first element is used\n")
		tms <- tms[1]
	}
	
	#simple user-input check before creating tiles
	if(ask)
	{
		ansind <- 0
		ansincorr <- 0
		while(ansind == 0 & ansincorr < 3)
		{
			ans <- readline(paste("This will create map tiles in \"", title, "\" folder\n up to a maximum zoom level of", max_zoom, ".\nDo you wish to continue (y/n)?\n"))
			if(!all(match(ans, c("y", "n"))))
			{
				cat("Incorrect input\n")
				ansincorr <- ansincorr + 1
			}
			else ansind <- 1
		}
		if(ansind == 0)
		{
			cat("Too many incorrect inputs, quitting function...\n")
			return(1)
		}
	}
		
	#convert object to latitude/longitude
	if(proj4string(object) != "+proj=longlat +ellps=WGS84") object <- spTransform(object, CRS("+proj=longlat"))
	
	#extract bounding box
	bounds <- as.numeric(t(bbox(object)))
	
	#find minimum zoom level that encompasses entire shapefile
	zoom <- findMinZoom(bounds)
	
	#now find starter tiles and coordinates for this zoom level
	start_tiles <- fitBounds(bounds, zoom)
	
	#convert object to Mercator projection for plotting
	object <- spTransform(object, CRS("+proj=merc"))
	
	#create tiles
	createTilesWorker(object, start_tiles, zoom, max_zoom, title, tms)
}
