#method to produce map tiles from SpatialPolygonsTiles object
setMethod("tiles", signature(object = "SpatialPolygonsTiles"),
	function(object, title, min_zoom = 1, max_zoom = 10, tms = FALSE, ask = TRUE, produce_html = TRUE, spher_merc = TRUE, ...) createTiles(object, title, min_zoom, max_zoom , tms, ask, produce_html, spher_merc, ...))
	
#method to amend subsetting to include subsetting tiles
setMethod("[", "SpatialPolygonsTiles", function(x, i, j, ... , drop = TRUE)
{
	#subset the SpatialPolygonsDataFrame object
	temp <- convertFromTileObject(x)
	temp <- temp[i, j, ..., drop = FALSE]
	
	#subset the tiles
	tiles <- x@tiles$coords
	tiles <- tiles[i]
	tiles <- list(min_zoom = x@tiles$min_zoom, max_zoom = x@tiles$max_zoom, coords = tiles)
		
	#convert back to SpatialPolygonsTiles object	
	x <- convertToTileObject_internal(temp, tiles)
	x
})

