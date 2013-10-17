\name{calcTileCoords}
\alias{calcTileCoords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcTileCoords(coords, zoom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{
%%     ~~Describe \code{coords} here~~
}
  \item{zoom}{
%%     ~~Describe \code{zoom} here~~
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
function (coords, zoom) 
{
    nrows <- 2^zoom
    top_left <- TileNumToLongLat(coords[1], coords[2], zoom)
    bottom_right <- TileNumToLongLat(coords[1] + 1, coords[2] + 
        1, zoom)
    if (coords[1] > nrows) 
        bottom_right[1] <- 180
    if (coords[2] > nrows) 
        bottom_right[2] <- -atan(sinh(pi)) * 180/2
    coords <- c(top_left, bottom_right)
    coords <- coords[c(1, 3, 4, 2)]
    coords
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
