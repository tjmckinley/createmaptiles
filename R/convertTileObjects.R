#function that takes a SpatialPolygonsDataFrame object and produces a SpatialPolygonsTiles
#object

convertToTileObject <- function(object, min_zoom = 1, max_zoom = 18)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(class(object) != "SpatialPolygonsDataFrame") stop("'object' not a 'SpatialPolygonsDataFrame' object\n")
	
	#convert object to latitude/longitude
	if(proj4string(object) != "+proj=longlat +ellps=WGS84") object <- spTransform(object, CRS("+proj=longlat"))
	
	res <- new("SpatialPolygonsTiles")
	res@bbox <- object@bbox
	res@proj4string <- object@proj4string
	res@plotOrder <- object@plotOrder
	res@data <- object@data
	res@polygons <- object@polygons
	res@tiles <- fitBounds(object, min_zoom, max_zoom)
	res
}

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
