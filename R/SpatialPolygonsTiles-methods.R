#method to produce map tiles from SpatialPolygonsTiles object
setMethod("tiles", signature(object = "SpatialPolygonsTiles"),
	function(object, ...) createTiles(object, ...))
