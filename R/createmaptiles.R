#uses information from http://wiki.openstreetmap.org

library(sp)
library(rgdal)

#function to convert longitude-latitude to tile numbers in Mercator projection
LongLatToTileNum <- function(lon_deg, lat_deg, zoom)
{
	lat_rad <- lat_deg * pi / 180
	n <- 2 ^ zoom
	xtile <- n * ((lon_deg + 180) / 360)
	ytile <- n * (1 - (log(tan(lat_rad) + (1 / cos(lat_rad))) / pi)) / 2
	c(xtile, ytile)
}

#function to convert tile number to longitude-latitude in Mercator projection
TileNumToLongLat <- function(xtile, ytile, zoom)
{
	n <- 2 ^ zoom
	lon_deg <- ((xtile / n) * 360.0) - 180.0
	lat_rad <- atan(sinh(pi * (1 - (2 * ytile / n))))
	lat_deg <- lat_rad * 180.0 / pi
	c(lon_deg, lat_deg)
}

#function to check whether one rectangle is contained within another
containsBounds <- function(tile, bounds)
{
	if(tile[2] < bounds[1]) return(0)
	else
	{
		if(tile[1] > bounds[2]) return(0)
		else
		{
			if(tile[4] < bounds[3]) return(0)
			else
			{
				if(tile[3] > bounds[4]) return(0)
				else
				{
					if(tile[1] < bounds[1] & tile[2] > bounds[2] & tile[3] < bounds[3] & tile[4] > bounds[4]) return(1)
					else return(0.5)
				}
			}
		}
	}
}

#function to calculate the area of a rectangle
areaRect <- function(rect)
{
	lengthx <- abs(diff(rect[1:2]))
	lengthy <- abs(diff(rect[3:4]))
	return(lengthx * lengthy)
}

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
	coords <- coords[c(1, 3, 4, 2)]
	coords
}

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
	#convert coordinates to TMS
	coords[, 2] <- nrows - coords[,2] - 1
	list(coords = coords, longlat = coords1)
}

#function to create tiles
createTiles <- function(object, title, max_zoom = 18)
{
	#convert object to latitude/longitude
	if(proj4string(object) != "+proj=longlat +ellps=WGS84") object <- spTransform(object, CRS("+proj=longlat"))
	
	#extract bounding box
	bounds <- as.numeric(t(bbox(object)))
	
	#find minimum zoom level that encompasses entire shapefile
	zoom <- findMinZoom(bounds)
	
	#now find starter tiles and coordinates for this zoom level
	start_tiles <- fitBounds(bounds, zoom)
	
	#convert object to Mercator projection for plotting
	object <- spTransform(object, CRS("+proj=merc"))
	
	#create tiles
	createTilesWorker(object, start_tiles, zoom, max_zoom, title)
}

#function to create maptiles
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
