#function to convert longitude-latitude to tile numbers in Mercator projection
LongLatToTileNum <- function(lon_deg, lat_deg, zoom)
{
	lat_rad <- lat_deg * pi / 180
	n <- 2 ^ zoom
	xtile <- n * ((lon_deg + 180) / 360)
	ytile <- n * (1 - (log(tan(lat_rad) + (1 / cos(lat_rad))) / pi)) / 2
	c(xtile, ytile)
}

