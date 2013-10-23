createmaptiles
==============

R package for creating scalable map tiles based on geo-referenced polygon data.

This extends the SpatialPolygonsDataFrame class defined in the "sp" package to produce a new SpatialPolygonsTiles class. This new class contains an additional "tiles" slot, that maps each polygon to each corresponding tile at each required zoom level. A "tiles" method then produces scalable map tiles for a range of zoom levels, that can be specified in eitherTMS or OSM formats. A further function exists that will convert a directory already in one of these formats to the other one, and visa-versa.

### License

This package is licensed to you under the terms of the [GNU General Public License](http://www.gnu.org/licenses/gpl.html) version 3 or later.
