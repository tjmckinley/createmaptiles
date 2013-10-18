#function to determine tile coordinates that contain bounding box
#based on a given zoom level
fitBounds <- function(bounds, zoom)
{	
	#define range of map
	nrows <- 2 ^ zoom
	#find tile coordinates in form (xmin, xmax, ymin, ymax) at current zoom level
	coords <- expand.grid(0:(nrows - 1), 0:(nrows - 1))
	coords1 <- t(apply(coords, 1, function(x, zoom) calcTileCoords(x, zoom), zoom = zoom))
	
	#now check whether bounding box is contained in tile or not
	fits <- apply(coords1, 1, function(x, bounds) containsBounds(x, bounds), bounds = bounds)
	
	#now extract tile coordinates relating to those tiles
	#(partially-) containing the bounding box
	coords <- coords[fits > 0, ]
	coords1 <- coords1[fits > 0, ]
	#output list of coordinates and tiles
	list(coords = coords, longlat = coords1)
}

