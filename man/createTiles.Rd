\name{createTiles}
\alias{createTiles}
\title{Produces map tiles from a 'SpatialPolygonsDataFrame' object}
\usage{
  createTiles(object, title, max_zoom = 18)
}
\arguments{
  \item{object}{a SpatialPolygonsDataFrame object.}

  \item{title}{a character specifying the name of the map
  tile folder to save to.}

  \item{max_zoom}{a numeric specifying the maximum zoom
  level to create tiles for (should range between 0-18.}
}
\description{
  Takes a 'Spatial' object, converts it to Spherical
  Mercator projection and then generates tiles at different
  zoom levels. At the current time the minimum zoom level
  is set to be the largest zoom such that the entire
  bounding box of the 'Spatial' object can be viewed in one
  window at that level. Currently, the function outputs
  tiles in TMS format.
}
\author{
  TJ McKinley
}
\references{
  http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
}

