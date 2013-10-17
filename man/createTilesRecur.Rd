\name{createTilesRecur}
\alias{createTilesRecur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createTilesRecur(object, start_coords, zoom, title, max_zoom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{start_coords}{
%%     ~~Describe \code{start_coords} here~~
}
  \item{zoom}{
%%     ~~Describe \code{zoom} here~~
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
function (object, start_coords, zoom, title, max_zoom) 
{
    if (zoom < max_zoom) 
        ind <- 0
    else ind <- 1
    zoom <- zoom + 1
    while (ind == 0) {
        start_coords[, 2] <- (2^(zoom - 1)) - start_coords[, 
            2] - 1
        coords <- apply(start_coords, 1, function(x) {
            coords <- matrix(c(2 * x[1], 2 * x[2], (2 * x[1]) + 
                1, 2 * x[2], 2 * x[1], (2 * x[2]) + 1, (2 * x[1]) + 
                1, (2 * x[2]) + 1), 4, 2, byrow = TRUE)
            list(coords)
        })
        coords <- do.call("rbind", lapply(coords, function(x) x[[1]]))
        tiles <- t(apply(coords, 1, function(x, zoom) calcTileCoords(x, 
            zoom), zoom = zoom))
        tiles[, 1:2] <- tiles[, 1:2] * pi/180
        tiles[, 3:4] <- tiles[, 3:4] * pi/180
        tiles[, 3:4] <- log(tan(tiles[, 3:4]) + (1/cos(tiles[, 
            3:4])))
        tiles <- tiles * 6378137
        coords[, 2] <- (2^zoom) - coords[, 2] - 1
        dir.create(paste0(title, "/", zoom))
        for (i in unique(coords[, 1])) dir.create(paste(c(title, 
            zoom, i), collapse = "/"))
        for (i in 1:nrow(coords)) {
            pathtofile <- paste(c(title, zoom, coords[i, 1]), 
                collapse = "/")
            png(paste0(pathtofile, "/", coords[i, 2], ".png"), 
                width = 248, height = 248, bg = "transparent")
            par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
            plot(object, xlim = tiles[i, 1:2], ylim = tiles[i, 
                3:4])
            dev.off()
        }
        ind <- createTilesRecur(object, coords, zoom, title, 
            max_zoom)
    }
    ind
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
