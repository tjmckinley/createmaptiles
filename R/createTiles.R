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
