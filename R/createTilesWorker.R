#worker function to create maptiles
createTilesWorker <- function(object, start_tiles, start_zoom, max_zoom, title)
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
	dir.create(paste0(title, "/", start_zoom))
	#create sub-directories based on x- and y-coordinates
	for(i in unique(start_coords[, 1]))	dir.create(paste(c(title, start_zoom, i), collapse = "/"))
			
	#now plot into directories
	for(i in 1:nrow(start_coords))
	{
		pathtofile <- paste(c(title, start_zoom, start_coords[i, 1]), collapse = "/")
		png(paste0(pathtofile, "/", start_coords[i, 2], ".png"), width = 248, height = 248, bg = 'transparent')
		par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
		plot(object, xlim = start_tiles[i, 1:2], ylim = start_tiles[i, 3:4])
		dev.off()
	}
	
	#now cycle through zoom-levels using a recursive function
	ind <- createTilesRecur(object, start_coords, start_zoom, title, max_zoom)
	
	if(ind == 1) cat("Tiles created successfully\n")
	else cat("Error in creation of tiles!\n")
}
