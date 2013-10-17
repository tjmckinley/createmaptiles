#function to convert tile number to longitude-latitude in Mercator projection
TileNumToLongLat <- function(xtile, ytile, zoom)
{
	n <- 2 ^ zoom
	lon_deg <- ((xtile / n) * 360.0) - 180.0
	lat_rad <- atan(sinh(pi * (1 - (2 * ytile / n))))
	lat_deg <- lat_rad * 180.0 / pi
	c(lon_deg, lat_deg)
}

