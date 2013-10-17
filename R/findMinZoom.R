#function to find minimum zoom level that could accommodate bounding box
findMinZoom <- function(bounds)
{
	min_zoom <- sapply(as.list(1:18), function(x)
	{
		#define range of map
		nrows <- 2 ^ x
		xdiff <- 180 * 2 / nrows
		ydiff <- (atan(sinh(pi)) * 2 / nrows) * (180 / pi)
		#compare size of tile to size of bounding box
		area_comp <- areaRect(bounds) - (xdiff * ydiff)
		if(area_comp > 0) return(0)
		else return(1)
	})
	min_zoom <- which(min_zoom == 1)
	if(length(min_zoom) == 0) return(0)
	else return(min_zoom[length(min_zoom)])
}
