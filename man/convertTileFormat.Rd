\name{convertTileFormat}
\alias{convertTileFormat}
\title{Converts existing map tile directory between OSM and TMS specifications}
\usage{
  convertTileFormat(dir)
}
\arguments{
  \item{dir}{a character denoting the path to the tile
  directory.}
}
\description{
  Takes an existing map directory of the form
  "dir/{z}/{x}/{y}.filetype", and switches format of
  directory between OSM and TMS formats. The function
  cannot check the format of the directory as it's passed
  to the function, but mistakes can be rectified by a
  reverse call.
}
\author{
  TJ McKinley
}
\references{
  http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
}

