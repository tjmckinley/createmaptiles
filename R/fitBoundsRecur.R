#recursive function to determine tile coordinates that contain bounding box
#based on a given zoom level
fitBoundsRecur <- function(bounds, cur_coords, zoom, max_zoom)
{			
	if(zoom <= max_zoom)
	{
		#find tile coordinates in form (xmin, xmax, ymin, ymax) at current zoom level
		coords <- apply(cur_coords, 1, function(x)
		{
			list(matrix(c(2 * x[1], 2 * x[2],
			2 * x[1] + 1, 2 * x[2],
			2 * x[1], 2 * x[2] + 1,
			2 * x[1] + 1, 2 * x[2] + 1), ncol = 2, byrow = TRUE))
		})
		coords <- do.call("rbind", lapply(coords, function(x) x[[1]]))
		coords1 <- t(apply(coords, 1, function(x, zoom) calcTileCoords(x, zoom), zoom = zoom))
	
		#now check whether bounding box is contained in tile or not
		fits <- apply(coords1, 1, function(x, bounds) containsBounds(x, bounds), bounds = bounds)
	
		#now extract tile coordinates relating to those tiles
		#(partially-) containing the bounding box
		coords <- coords[fits > 0, , drop = FALSE]
	
		if(nrow(coords) > 0)
		{
			#now pass these tiles onto a recursive function to calculate
			#tiles at each further zoom level
			prev_coords <- fitBoundsRecur(bounds, coords, zoom + 1, max_zoom)
			#append zoom level
			coords <- cbind(coords, rep(zoom, nrow(coords)))
			if(!is.null(prev_coords)) return(rbind(prev_coords, coords))
			else return(coords)
		}
		else return(NULL)
	}
}

