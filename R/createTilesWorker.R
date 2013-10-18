#worker function to create maptiles
createTilesWorker <- function(object, start_tiles, start_zoom, max_zoom, title, tms)
{
	#extract elements of interest from start_tiles
	start_coords <- start_tiles$coords
	start_tiles <- start_tiles$longlat
	
	#transform bounding box to Mercator projection for plotting
	start_tiles[, 1:2] <- start_tiles[, 1:2] * pi / 180
	start_tiles[, 3:4] <- start_tiles[, 3:4] * pi / 180
	start_tiles[, 3:4] <- log(tan(start_tiles[, 3:4]) + (1 / cos(start_tiles[, 3:4])))
	start_tiles <- start_tiles * 6378137
	
	#create output directory
	dir.create(title)
	#create directories based on initial tiles
	dir.create(file.path(title, start_zoom))
	#create sub-directories based on x- and y-coordinates
	for(i in unique(start_coords[, 1]))	dir.create(file.path(title, start_zoom, i))
	
	#convert coordinates to TMS if required
	if(tms == TRUE)
	{
		nrows <- 2 ^ start_zoom
		start_coords[, 2] <- nrows - start_coords[, 2] - 1
	}
			
	#now plot into directories
	for(i in 1:nrow(start_coords))
	{
		pathtofile <- file.path(title, start_zoom, start_coords[i, 1])
		png(file.path(pathtofile, paste0(start_coords[i, 2], ".png")), width = 248, height = 248, bg = 'transparent')
		par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
		plot(object, xlim = start_tiles[i, 1:2], ylim = start_tiles[i, 3:4])
		dev.off()
	}
	
	#now cycle through zoom-levels using a recursive function
	ind <- createTilesRecur(object, start_coords, start_zoom, title, max_zoom, tms)
	
	if(ind == 1) cat("Tiles created successfully\n")
	else cat("Error in creation of tiles!\n")
}
