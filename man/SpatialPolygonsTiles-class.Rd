\name{SpatialPolygonsTiles-class}
\docType{class}
\alias{SpatialPolygonsTiles-class}
\alias{SpatialPolygonsTiles}
\alias{tiles,SpatialPointsTiles-method}
\alias{"[",SpatialPointsTiles-method}

\title{Class "SpatialPolygonsTiles" }
\description{class to hold polygons with attributes, which can be used to generate scalable map tiles}
\details{
The \code{tiles} method for SpatialPolygonTiles takes the following arguments:
\describe{
  \item{x}{a SpatialPolygonsTiles object}
  \item{title}{a character denoting the path to the folder where the tiles are to be created; folder does not need to exist}
  \item{max_zoom}{default 18; the maximum zoom level required (ranges between 1--18)}
  \item{tms}{default FALSE; sets map tile coordinates to TMS or OSM specification}
  \item{ask}{default TRUE; asks user to clarify before creating tiles (set to FALSE if running in batch mode)}
  \item{produce_html}{default TRUE; produces a simple webpage in the 'title' folder (index.html) to display scalable map. The page is build using the Leaflet javascript library.}
  \item{spher_merc}{default TRUE; transforms current projection into Spherical Mercator for use with GoogleMaps API etc.}
  \item{...}{other arguments passed through}
}
Note that this method can only take additional arguments for 'col, 'border', 'density', 'angle', 'pbg' and 'lty'. The \code{plot} method works in the same way as for \link{SpatialPolygonsDataFrame} objects.
}
\section{Objects from the Class}{
Objects can be created by calls to the function \link{convertToTileObject}, and the tile information removed by a call to \link{convertFromTileObject}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}; attribute table }
    \item{\code{polygons}:}{Object of class \code{"list"}; see \link{SpatialPolygons-class} }
    \item{\code{plotOrder}:}{Object of class \code{"integer"}; see \link{SpatialPolygons-class} }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class}}
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
    \item{\code{tiles}:}{Object of class \code{"list"}; see \link{convertToTileObject}}
  }
}
\section{Extends}{
Class \code{SpatialPolygonsDataFrame} from the 'sp' package, directly.
}
\section{Methods}{
See \code{\linkS4class{SpatialPolygonsDataFrame}} for inherited methods.
  \describe{
    \item{[}{\code{signature(x = "SpatialPolygonsTiles")}: select subset of (sets of) polygons; NAs are not permitted in the row index}
    \item{tiles}{\code{signature(x = "SpatialPolygonsTiles")}: produce a set of map tiles from object.}
  }
}
\author{TJ McKinley}
\seealso{
	\link{SpatialPolygons-class}
	\link{SpatialPolygonsDataFrame-class}
}
\references{
	http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
	http://leafletjs.com/
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
\keyword{classes}
