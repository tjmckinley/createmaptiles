#function that takes a SpatialPolygonsDataFrame object and produces map tiles

createTiles <- function(object, title, min_zoom, max_zoom = 18, tms = FALSE, ask = TRUE, ...)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(missing(title)) stop("'title' missing\n")
	if(missing(max_zoom)) stop("'max_zoom' missing\n")
	if(class(object) != "SpatialPolygonsTiles") stop("'object' not a 'SpatialPolygonsTiles' object\n")
	if(!is.character(title[1])) stop("'title' is not a character\n")
	if(length(title) > 1)
	{
		cat("'title' has length > 1, so only first element is used\n")
		title <- title[1]
	}
	if(!is.numeric(max_zoom[1])) stop("'max_zoom' is not numeric\n")
	if(length(max_zoom) > 1)
	{
		cat("'max_zoom' has length > 1, so only first element is used\n")
		max_zoom <- max_zoom[1]
	}
	if(round(max_zoom) != max_zoom) stop("'max_zoom' is not an integer\n")
	if(max_zoom < 0 | max_zoom > 18) stop("'max_zoom' not in the range 0-18\n")
	if(!is.logical(tms)) stop("'tms' is not a logical value")
	if(length(tms) > 1)
	{
		cat("'tms' has length > 1, so only first element is used\n")
		tms <- tms[1]
	}
	
	#check min and max zoom levels match against those found in the SpatialPolygonsTiles object
	if(min_zoom < object@tiles$min_zoom | max_zoom > object@tiles$max_zoom)
	{
		stop(paste("'min_zoom' and 'max_zoom' from SpatialPolygonsTiles object are:", object@tiles$min_zoom, "and", object@tiles$max_zoom, ",\n values chosen in function are:", min_zoom, "and", max_zoom, ".\n"))
	}
	
	#simple user-input check before creating tiles
	if(ask)
	{
		ansind <- 0
		ansincorr <- 0
		while(ansind == 0 & ansincorr < 3)
		{
			ans <- readline(paste0("\nThis will create map tiles in \"", title, "\" folder;\nfrom a minimum zoom level of ", min_zoom, ", up to a maximum zoom level of ", max_zoom, ".\nThe files will be saved in ", ifelse(tms == "tms", "TMS", "OSM"), " specification.\nDo you wish to continue (y/n)?\n"))
			if(!all(match(ans, c("y", "n"))))
			{
				cat("Incorrect input\n")
				ansincorr <- ansincorr + 1
			}
			else ansind <- 1
		}
		if(ansind == 0)	stop("Too many incorrect inputs, quitting function...\n")
		if(ans == "n") stop("Quitting function.\n")
	}
	
	#now create directory structure
	if(!dir.create(title)) stop(paste(title, "directory already exists\n"))
	
	#check object is in Mercator projection
	if(proj4string(object) != "+proj=merc +ellps=WGS84") stop("'object' not in correct projection (+proj=merc +ellps=WGS84)")
	
	#now generate all tiles to render
	tiles <- object@tiles$coords
	temp_tiles <- sapply(tiles, nrow)
	temp_tiles <- rep(1:length(temp_tiles), times = temp_tiles)
	tiles <- do.call("rbind", tiles)
	tiles <- cbind(tiles, temp_tiles)
	
	#extract tiles in between minimum and maximum zoom levels
	tiles <- tiles[tiles[, 3] >= min_zoom, ]
	tiles <- tiles[tiles[, 3] <= max_zoom, ]
	
	#match polygons to tiles
	rownames(tiles) <- 1:nrow(tiles)
	tiles <- as.data.frame(tiles)
	tiles <- cbind(tiles, apply(tiles, 1, function(x) paste(x[1:3], collapse = " ")))
	tiles[, 5] <- factor(tiles[, 5])
	tiles_polys <- tapply(tiles[, 4], tiles[, 5], function(x) x)
	
	#extract unique tiles
	tiles <- tiles[!duplicated(tiles[, 5]), ]
	
	for(i in unique(tiles[, 3]))
	{
		#create sub-directory based on zoom level
		dir.create(file.path(title, i))
		#create further sub-directories based on x-coordinates
		temp <- tiles[tiles[, 3] == i, ]
		for(j in unique(temp[, 1])) dir.create(file.path(title, i, j))
	}
	
	#now render tiles
	for(i in 1:nrow(tiles))
	{
		#calculate tile coordinates in longitude/latitude
		temp_tiles <- calcTileCoords(tiles[i, 1:2], tiles[i, 3])
		
		#extract only those polygons contained in tile for plotting
		temp_polys <- tiles_polys[match(tiles[i, 5], names(tiles_polys))][[1]]
		temp_polys <- object[temp_polys, ]
	
		#convert y-coordinates to TMS if required
		if(tms == TRUE)	tiles[i, 2] <- (2 ^ tiles[i, 3]) - tiles[i, 2] - 1
	
		#now produce plot
		pathtofile <- file.path(title, tiles[i, 3], tiles[i, 1])
		png(file.path(pathtofile, paste0(tiles[i, 2], ".png")), width = 248, height = 248, bg = 'transparent')
		par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
		plot(temp_polys, xlim = temp_tiles[1:2], ylim = temp_tiles[3:4], ...)
		dev.off()
	}
}
