\name{convertTileObjects}
\alias{convertTileObjects}
\alias{convertToTileObject}
\alias{convertFromTileObject}

\title{Converts to and from SpatialPolygonsTiles objects}
\description{These functions will convert a SpatialPolygonsDataFrame object to a SpatialPolygonsTiles object, or visa-versa.}
\usage{
	convertToTileObject(object, min_zoom = 1, max_zoom = 18)
	convertFromTileObject(object)
}
\arguments{
  \item{object}{a \link{SpatialPolygonsDataFrame} object (for convertTo*), or a \link{SpatialPolygonsTiles} object (for convertFrom*)}
  \item{min_zoom}{default 1; the minimum zoom level required (ranges between 1--18)}
  \item{max_zoom}{default 18; the maximum zoom level required (ranges between 1--18)}
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
}
\examples{
}
