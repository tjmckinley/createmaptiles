#function that takes a SpatialPolygonsDataFrame object and produces map tiles

#' Produces map tiles from a 'SpatialPolygonsDataFrame' object
#' 
#' Takes a 'Spatial' object, converts it to Spherical Mercator projection
#' and then generates tiles at different zoom levels. At the current time
#' the minimum zoom level is set to be the largest zoom such that the entire
#' bounding box of the 'Spatial' object can be viewed in one window at that
#' level. Currently, the function outputs tiles in TMS format.
#' 
#' @param object a SpatialPolygonsDataFrame object.
#' @param title a character specifying the name of the map tile folder to save to.
#' @param max_zoom a numeric specifying the maximum zoom level to create tiles for (should
#' range between 0-18.
#' @author TJ McKinley
#' @references http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
#'
#' @export createTiles

createTiles <- function(object, title, max_zoom = 18)
{
	#some checks to ensure correct inputs
	if(missing(object))
	{
		cat("'object' missing\n")
		return(1)
	}
	if(missing(title))
	{
		cat("'title' missing\n")
		return(1)
	}
	if(missing(max_zoom))
	{
		cat("'max_zoom' missing\n")
		return(1)
	}
	if(class(object) != "SpatialPolygonsDataFrame")
	{
		cat("'object' not a 'SpatialPolygonsDataFrame' object\n")
		return(1)
	}
	if(!is.character(title[1]))
	{
		cat("'title' is not a character\n")
		return(1)
	}
	if(length(title) > 1)
	{
		cat("'title' has length > 1, so only first element is used\n")
		title <- title[1]
	}
	if(!is.numeric(max_zoom[1]))
	{
		cat("'max_zoom' is not numeric\n")
		return(1)
	}
	if(length(max_zoom) > 1)
	{
		cat("'max_zoom' has length > 1, so only first element is used\n")
		max_zoom <- max_zoom[1]
	}
	if(round(max_zoom) != max_zoom)
	{
		cat("'max_zoom' is not an integer\n")
		return(1)
	}
	if(max_zoom < 0 | max_zoom > 18)
	{
		cat("'max_zoom' not in the range 0-18\n")
		return(1)
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
	createTilesWorker(object, start_tiles, zoom, max_zoom, title)
}
