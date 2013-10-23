#function that takes a SpatialPolygonsDataFrame object and produces a SpatialPolygonsTiles
#object

convertToTileObject <- function(object, min_zoom = 1, max_zoom = 18)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(class(object) != "SpatialPolygonsDataFrame") stop("'object' not a 'SpatialPolygonsDataFrame' object\n")
	
	#convert object to latitude/longitude
	if(proj4string(object) != "+proj=merc") object <- spTransform(object, CRS("+proj=merc"))
	
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

