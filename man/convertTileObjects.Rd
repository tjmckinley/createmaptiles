\name{convertTileObjects}
\alias{convertTileObjects}
\alias{convertToTileObject}
\alias{convertFromTileObject}

\title{Converts to and from SpatialPolygonsTiles objects}
\description{These functions will convert a SpatialPolygonsDataFrame object to a SpatialPolygonsTiles object, or visa-versa.}
\usage{
	convertToTileObject(object, min_zoom = 1, max_zoom = 10)
	convertFromTileObject(object)
}
\arguments{
  \item{object}{a \link{SpatialPolygonsDataFrame} object (for convertTo*), or a \link{SpatialPolygonsTiles} object (for convertFrom*)}
  \item{min_zoom}{default 1; the minimum zoom level required (ranges between 1--18)}
  \item{max_zoom}{default 10; the maximum zoom level required (ranges between 1--18)}
}
\details{
Converts object into Spherical Mercator projection using details specified at http://proj.maptools.org/faq.html. This should render tiles compatible with GoogleMaps API etc.
}
\value{
Returns a \link{SpatialPolygonsDataFrame} or \link{SpatialPolygonsTiles} object
}
\author{TJ McKinley}
\seealso{
	\link{SpatialPolygons-class}
	\link{SpatialPolygonsDataFrame-class}
	\link{SpatialPolygonsTiles-class}
}
\references{
	http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
	http://proj.maptools.org/faq.html
}
\examples{
#load UK administration states shapefile using 'rgdal' package
#(generated from Natural Earth: see http://www.naturalearthdata.com)
pathtoshapefile <- find.package("createmaptiles")
pathtoshapefile <- file.path(pathtoshapefile,"extdata","uk")
uk.shp <- readOGR(pathtoshapefile, "uk")

#convert to SpatialPolygonsTiles object
uk.tl <- convertToTileObject(uk.shp, max_zoom = 6)

#produce map tiles in "uk" folder in current working directory
#(once the function has run, the file './uk/index.html' allows
#the map tiles to be explored)
tiles(uk.tl, "uk", max_zoom = 6)

#now randomly colour polygons
tiles(uk.tl, "uk_col", max_zoom = 6, col = topo.colors(64)[sample(1:64, nrow(uk.tl), replace = TRUE)])
}
