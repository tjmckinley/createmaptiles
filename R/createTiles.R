#function that takes a SpatialPolygonsDataFrame object and produces map tiles

createTiles <- function(object, title, min_zoom = 1, max_zoom = 18, tms = FALSE, ask = TRUE, produce_html = TRUE, spher_merc = TRUE, ...)
{
	#some checks to ensure correct inputs
	if(missing(object)) stop("'object' missing\n")
	if(missing(title)) stop("'title' missing\n")
#	if(missing(min_zoom)) stop("'min_zoom' missing\n")
#	if(missing(max_zoom)) stop("'max_zoom' missing\n")
#	if(missing(tms)) stop("'tms' missing\n")
#	if(missing(ask)) stop("'ask' missing\n")
#	if(missing(produce_html)) stop("'produce_html' missing\n")
#	if(missing(spher_merc)) stop("'spher_merc' missing\n")
	if(class(object) != "SpatialPolygonsTiles") stop("'object' not a 'SpatialPolygonsTiles' object\n")
	if(!is.character(title[1])) stop("'title' is not a character\n")
	if(length(title) > 1)
	{
		cat("'title' has length > 1, so only first element is used\n")
		title <- title[1]
	}
	if(!is.numeric(min_zoom[1])) stop("'min_zoom' is not numeric\n")
	if(length(min_zoom) > 1)
	{
		cat("'min_zoom' has length > 1, so only first element is used\n")
		min_zoom <- min_zoom[1]
	}
	if(round(min_zoom) != min_zoom) stop("'min_zoom' is not an integer\n")
	if(min_zoom < 1 | min_zoom > 18) stop("'min_zoom' not in the range 1-18\n")
	if(!is.numeric(max_zoom[1])) stop("'max_zoom' is not numeric\n")
	if(length(max_zoom) > 1)
	{
		cat("'max_zoom' has length > 1, so only first element is used\n")
		max_zoom <- max_zoom[1]
	}
	if(round(max_zoom) != max_zoom) stop("'max_zoom' is not an integer\n")
	if(max_zoom < 1 | max_zoom > 18) stop("'max_zoom' not in the range 1-18\n")
	if(!is.logical(tms)) stop("'tms' is not a logical value")
	if(length(tms) > 1)
	{
		cat("'tms' has length > 1, so only first element is used\n")
		tms <- tms[1]
	}
	if(!is.logical(ask)) stop("'ask' is not a logical value")
	if(length(ask) > 1)
	{
		cat("'ask' has length > 1, so only first element is used\n")
		ask <- ask[1]
	}
	if(!is.logical(produce_html)) stop("'produce_html' is not a logical value")
	if(length(produce_html) > 1)
	{
		cat("'produce_html' has length > 1, so only first element is used\n")
		produce_html <- produce_html[1]
	}
	if(!is.logical(spher_merc)) stop("'spher_merc' is not a logical value")
	if(length(spher_merc) > 1)
	{
		cat("'spher_merc' has length > 1, so only first element is used\n")
		spher_merc <- spher_merc[1]
	}
	#if necessary, convert to Spherical Mercator projection
	if(spher_merc == TRUE)
	{
		if(proj4string(object) != "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs")
		{
			#convert to SpatialPolygonsDataFrame object
			temp <- convertFromTileObject(object)
			
			#transform to Spherical Mercator projection
			temp <- spTransform(temp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))
	
			#rebind the tiles
			tiles <- object@tiles$coords
			tiles <- list(min_zoom = object@tiles$min_zoom, max_zoom = object@tiles$max_zoom, coords = tiles)
		
			#convert back to SpatialPolygonsTiles object	
			object <- convertToTileObject_internal(temp, tiles)
		}	
	}
	
	#extract only relevant elements of "..." argument
	dots <- list(...)
	allowed_args <- c("col", "border", "density", "angle", "pbg", "lty")
	dots <- dots[!is.na(match(names(dots), allowed_args))]
	specified_args <- NULL
	specified_args_full <- NULL
	#check these args if they exist
	for(i in names(dots))
	{
		if(i != "pbg")
		{
			temp <- dots[names(dots) == i][[1]]
			if(length(temp) != 1 & length(temp) != nrow(object)) stop(paste0("length of '",i, "' argument incorrect"))
			#append information to data attributes to account for subsetting
			#(call these columns "*_blah" and check they're unique
			if(length(temp) > 1)
			{
				if(paste0(i, "_blah") %in% names(object)) stop(paste("Please rename", paste0(i, "_blah"), "column"))
				object[[paste0(i, "_blah")]] <- temp
				dots <- dots[-which(names(dots) == i)]
				specified_args <- c(specified_args, i)
				specified_args_full <- c(specified_args_full, paste0(i, "_blah"))
			}
		}
		if(i == "pbg")
		{
			if(length(dots[names(dots) == i][[1]]) != 1) stop("'pgb' argument not of length 1")
		}
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
	if(!dir.create(title))
	{
		if(ask)
		{
			ansind <- 0
			ansincorr <- 0
			while(ansind == 0 & ansincorr < 3)
			{
				ans <- readline(paste0("Do you want to overwrite (y/n)?\n"))
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
		cat(paste0("Overwriting '", title, "' directory...\n"))
		unlink(title, recursive = TRUE)
		dir.create(title)
	}
	
	#convert object to Spherical Mercator projection
	if(proj4string(object) != "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs")
	{
		cat("Converting object to Spherical Mercator projection...\n")
		object <- spTransform(object, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))
	}
	
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
		#create list of additional arguments to pass to plot
		temp_dots <- list(x = temp_polys, xlim = temp_tiles[1:2], ylim = temp_tiles[3:4])
		if(length(specified_args) > 0)
		{
			temp_args <- temp_polys@data[, match(specified_args_full, names(temp_polys)), drop = FALSE]
			if(ncol(temp_args) > 1) temp_args <- as.list(temp_args)
			else temp_args <-list(unlist(temp_args))
			names(temp_args) <- specified_args
			for(j in 1:length(temp_dots)) temp_args[[names(temp_dots)[j]]] <- temp_dots[[j]]
		}
		else temp_args <- temp_dots
		if(length(dots) > 0) for(j in 1:length(dots)) temp_args[[names(dots)[j]]] <- dots[[j]]
		#now plot according to arguments
		do.call(plot, temp_args)
		dev.off()
	}
	
	#if requested, produce a simple HTML page for checking
	if(produce_html == TRUE)
	{
		htmlpage <- find.package("createmaptiles")
		htmlpage <- file.path(htmlpage, "webpage", "index.html")
		htmlpage <- readLines(htmlpage)
		#set centre for map
		coords <- rev(apply(bbox(object), 1, mean))
		#convert to latitude and longitude
		coords <- coords / 6378137
		coords[1] <- atan(sinh(coords[1]))
		coords <- coords * 180 / pi
		htmlpage[32] <- sub("LAT", as.character(coords[1]), htmlpage[32])
		htmlpage[32] <- sub("LONG", as.character(coords[2]), htmlpage[32])
		#set filetype
		htmlpage[33] <- sub("FILETYPE", "png", htmlpage[33])
		#set zoom levels
		htmlpage[34] <- sub("MINZOOM", min_zoom, htmlpage[34])
		htmlpage[35] <- sub("MAXZOOM", max_zoom, htmlpage[35])
		#set specification
		htmlpage[36] <- sub("TMS", ifelse(tms == TRUE, "true", "false"), htmlpage[36])
		#write out html page to map tiles folder
		writeLines(htmlpage, paste0(file.path(title, "index.html")))
	}	
}
