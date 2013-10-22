setClass("SpatialPolygonsTiles",
	contains = "SpatialPolygonsDataFrame",
	representation = representation(
		tiles = "list"
	)
)

setGeneric("tiles", function(object, ...) standardGeneric("tiles"))
