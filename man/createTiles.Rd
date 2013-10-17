\name{createTiles}
\alias{createTiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createTiles(object, title, max_zoom = 18)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{max_zoom}{
%%     ~~Describe \code{max_zoom} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, title, max_zoom = 18) 
{
    if (proj4string(object) != "+proj=longlat +ellps=WGS84") 
        object <- spTransform(object, CRS("+proj=longlat"))
    bounds <- as.numeric(t(bbox(object)))
    zoom <- findMinZoom(bounds)
    start_tiles <- fitBounds(bounds, zoom)
    object <- spTransform(object, CRS("+proj=merc"))
    createTilesWorker(object, start_tiles, zoom, max_zoom, title)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
