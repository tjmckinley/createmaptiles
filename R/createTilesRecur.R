#recursive function to produce plots
createTilesRecur <- function(object, start_coords, zoom, title, max_zoom)
{
	if(zoom < max_zoom) ind <- 0
	else ind <- 1
	
	zoom <- zoom + 1
	
	while(ind == 0)
	{		
		#convert coordinates to TMS
		start_coords[, 2] <- (2 ^ (zoom - 1)) - start_coords[, 2] - 1
		
		#create coordinates for next zoom level
		coords <- apply(start_coords, 1, function(x)
		{
			#create matrix of subtile coordinates
			coords <- matrix(c(2 * x[1], 2 * x[2],
				(2 * x[1]) + 1, 2 * x[2],
				2 * x[1], (2 * x[2]) + 1,
				(2 * x[1]) + 1, (2 * x[2]) + 1), 4, 2, byrow = TRUE)
			list(coords)
		})
		coords <- do.call("rbind", lapply(coords, function(x) x[[1]]))
	
		#calculate tile coordinates
		tiles <- t(apply(coords, 1, function(x, zoom) calcTileCoords(x, zoom), zoom = zoom))
		
		#convert tiles to Marcator projection for plotting
		tiles[, 1:2] <- tiles[, 1:2] * pi / 180
		tiles[, 3:4] <- tiles[, 3:4] * pi / 180
		tiles[, 3:4] <- log(tan(tiles[, 3:4]) + (1 / cos(tiles[, 3:4])))
		tiles <- tiles * 6378137
				
		#convert coordinates to TMS
		coords[, 2] <- (2 ^ zoom) - coords[, 2] - 1
	
		#create directories based on initial tiles
		dir.create(paste0(title, "/", zoom))
		#create sub-directories based on x- and y-coordinates
		for(i in unique(coords[, 1])) dir.create(paste(c(title, zoom, i), collapse = "/"))
	
		#now plot into directories
		for(i in 1:nrow(coords))
		{
			pathtofile <- paste(c(title, zoom, coords[i, 1]), collapse = "/")
			png(paste0(pathtofile, "/", coords[i, 2], ".png"), width = 248, height = 248, bg = 'transparent')
			par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
			plot(object, xlim = tiles[i, 1:2], ylim = tiles[i, 3:4])
			dev.off()
		}
		#run next recursive function
		ind <- createTilesRecur(object, coords, zoom, title, max_zoom)
	}
	ind
}
