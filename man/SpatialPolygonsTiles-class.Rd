\name{SpatialPolygonsTiles-class}
\docType{class}
\alias{SpatialPolygonsTiles-class}

\title{Class "SpatialPolygonsTiles" }
\description{class to hold polygons with attributes, which can be used to generate scalable map tiles}
\details{
The \code{tiles} method for SpatialPolygonTiles takes the following arguments:
\describe{
  \item{x}{a SpatialPolygonsTiles object}
  \item{title}{a character denoting the path to the folder where the tiles are to be created; folder does not need to exist}
  \item{max_zoom}{default 18; the maximum zoom level required (ranges between 1--18)}
  \item{tms}{default "tms"; sets map tile coordinates to TMS or OSM specification}
  \item{ask}{default TRUE; asks user to clarify before creating tiles (set to FALSE if running in batch mode)}
  \item{...}{other arguments passed through}
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
    \item{\code{tileinfo}:}{Object of class \code{"list"}; see \link{createT}
  }
}
\section{Extends}{
Class \code{"SpatialPolygonsDataFrame"} from the 'sp' package, directly.
}
\section{Methods}{
Methods defined with class "SpatialPolygonsTiles" in the signature:
  \describe{
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
}
\examples{
# simple example, from scratch:
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")


}
\keyword{classes}
