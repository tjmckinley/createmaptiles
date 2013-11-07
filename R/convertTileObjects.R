#function that takes a SpatialPolygonsDataFrame object and produces a SpatialPolygonsTiles
#object

convertToTileObject <- function(object, min_zoom = 1, max_zoom = 10, spher_merc = TRUE)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
#	if(missing(min_zoom)) stop("'min_zoom' missing\n")
#	if(missing(max_zoom)) stop("'max_zoom' missing\n")
#	if(missing(spher_merc)) stop("'spher_merc' missing\n")
	if(class(object) != "SpatialPolygonsDataFrame") stop("'object' not a 'SpatialPolygonsDataFrame' object\n")
	if(!is.numeric(min_zoom[1])) stop("'min_zoom' is not numeric\n")
	if(length(min_zoom) > 1)
	{
		cat("'min_zoom' has length > 1, so only first element is used\n")
		min_zoom <- min_zoom[1]
	}
	if(round(min_zoom) != min_zoom) stop("'min_zoom' is not an integer\n")
	if(min_zoom < 1 | min_zoom > 18) stop("'min_zoom' not in the range 1-18\n")
	if(!is.numeric(max_zoom[1])) stop("'max_zoom' is not numeric\n")
	if(length(max_zoom) > 1)
	{
		cat("'max_zoom' has length > 1, so only first element is used\n")
		max_zoom <- max_zoom[1]
	}
	if(round(max_zoom) != max_zoom) stop("'max_zoom' is not an integer\n")
	if(max_zoom < 1 | max_zoom > 18) stop("'max_zoom' not in the range 1-18\n")
	if(!is.logical(spher_merc)) stop("'spher_merc' is not a logical value")
	if(length(spher_merc) > 1)
	{
		cat("'spher_merc' has length > 1, so only first element is used\n")
		spher_merc <- spher_merc[1]
	}
	
	#convert object to Spherical Mercator projection
	if(spher_merc == TRUE)
	{
		if(proj4string(object) != "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs") object <- spTransform(object, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))
	}
	
	res <- new("SpatialPolygonsTiles")
	res@bbox <- object@bbox
	res@proj4string <- object@proj4string
	res@plotOrder <- object@plotOrder
	res@data <- object@data
	res@polygons <- object@polygons
	res@tiles <- fitBounds(object, min_zoom, max_zoom)
	res
}

#function that takes a SpatialPolygonsTiles object and produces a SpatialPolygonsDataFrame
#object
convertFromTileObject <- function(object)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(class(object) != "SpatialPolygonsTiles") stop("'object' not a 'SpatialPolygonsTiles' object\n")
	res <- new("SpatialPolygonsDataFrame")
	res@bbox <- object@bbox
	res@proj4string <- object@proj4string
	res@plotOrder <- object@plotOrder
	res@data <- object@data
	res@polygons <- object@polygons
	res
}

#internal function to be used in subsetting
convertToTileObject_internal <- function(object, tiles = NA)
{		
	res <- new("SpatialPolygonsTiles")
	res@bbox <- object@bbox
	res@proj4string <- object@proj4string
	res@plotOrder <- object@plotOrder
	res@data <- object@data
	res@polygons <- object@polygons
	res@tiles <- tiles
	res
}

