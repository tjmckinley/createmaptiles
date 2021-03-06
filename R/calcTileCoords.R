#function to calculate tile coordinates
calcTileCoords <- function(coords, zoom)
{
	#define range of map
	nrows <- 2 ^ zoom
	
	#find top-left corner
	top_left <- TileNumToLongLat(coords[1], coords[2], zoom)

	#find bottom-right corner
	bottom_right <- TileNumToLongLat(coords[1] + 1, coords[2] + 1, zoom)
	if(coords[1] > nrows) bottom_right[1] <- 180
	if(coords[2] > nrows) bottom_right[2] <- -atan(sinh(pi)) * 180 / 2

	#append coordinates together to form tile coordinates
	coords <- c(top_left, bottom_right)
	coords <- as.numeric(coords[c(1, 3, 4, 2)])
	
	#transform tiles to Mercator projection for plotting
	coords[1:2] <- coords[1:2] * pi / 180
	coords[3:4] <- coords[3:4] * pi / 180
	coords[3:4] <- log(tan(coords[3:4]) + (1 / cos(coords[3:4])))
	coords <- coords * 6378137
	
	coords
}

