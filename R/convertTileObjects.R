#function that takes a SpatialPolygonsDataFrame object and produces a SpatialPolygonsTiles
#object

convertToTileObject <- function(object, min_zoom = 1, max_zoom = 10)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(class(object) != "SpatialPolygonsDataFrame") stop("'object' not a 'SpatialPolygonsDataFrame' object\n")
	
	#convert object to Spherical Mercator projection
	if(proj4string(object) != "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs") object <- spTransform(object, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))
	
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

