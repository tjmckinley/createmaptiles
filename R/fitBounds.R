#function to determine tile coordinates that contain bounding box
#based on a given zoom level
fitBounds <- function(object, min_zoom, max_zoom)
{				
	#define range of map
	nrows <- 2 ^ min_zoom
	
	#find tile coordinates in form (xmin, xmax, ymin, ymax) at current zoom level
	coords <- expand.grid(0:(nrows - 1), 0:(nrows - 1))
	coords1 <- t(apply(coords, 1, function(x, zoom) calcTileCoords(x, zoom), zoom = min_zoom))
	
	#now check whether bounding box is contained in tile or not, for each polygon
	output <- list(NULL)
	for(i in 1:nrow(object))
	{
		#extract bounding box of polygon
		bounds <- as.numeric(t(bbox(object[i, ])))
		
		#check which tiles the polygon lies in
		fits <- apply(coords1, 1, function(x, bounds) containsBounds(x, bounds), bounds = bounds)
	
		#now extract tile coordinates relating to those tiles
		#(partially-) containing the bounding box
		cur_coords <- coords[fits > 0, , drop = FALSE]
	
		#now pass these tiles onto a recursive function to calculate
		#tiles at each further zoom level
		prev_coords <- fitBoundsRecur(bounds, cur_coords, min_zoom + 1, max_zoom)
		
		#append zoom level
		cur_coords <- as.matrix(cbind(cur_coords, rep(min_zoom, nrow(cur_coords))))
		
		#output tiles matching to polygon
		if(!is.null(prev_coords)) output[[i]] <- rbind(prev_coords, cur_coords)
		else output[[i]] <- cur_coords
		
		#tidy up output
		colnames(output[[i]]) <- c("x", "y", "zoom")
		rownames(output[[i]]) <- 1:nrow(output[[i]])
	}
	output <- list(min_zoom = min_zoom, max_zoom = max_zoom, coords = output)
	output
}

