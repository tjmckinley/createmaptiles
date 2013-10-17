\name{createTilesWorker}
\alias{createTilesWorker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createTilesWorker(object, start_tiles, start_zoom, max_zoom, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{start_tiles}{
%%     ~~Describe \code{start_tiles} here~~
}
  \item{start_zoom}{
%%     ~~Describe \code{start_zoom} here~~
}
  \item{max_zoom}{
%%     ~~Describe \code{max_zoom} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
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
function (object, start_tiles, start_zoom, max_zoom, title) 
{
    start_coords <- start_tiles$coords
    start_tiles <- start_tiles$longlat
    start_tiles[, 1:2] <- start_tiles[, 1:2] * pi/180
    start_tiles[, 3:4] <- start_tiles[, 3:4] * pi/180
    start_tiles[, 3:4] <- log(tan(start_tiles[, 3:4]) + (1/cos(start_tiles[, 
        3:4])))
    start_tiles <- start_tiles * 6378137
    dir.create(title)
    dir.create(paste0(title, "/", start_zoom))
    for (i in unique(start_coords[, 1])) dir.create(paste(c(title, 
        start_zoom, i), collapse = "/"))
    for (i in 1:nrow(start_coords)) {
        pathtofile <- paste(c(title, start_zoom, start_coords[i, 
            1]), collapse = "/")
        png(paste0(pathtofile, "/", start_coords[i, 2], ".png"), 
            width = 248, height = 248, bg = "transparent")
        par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
        plot(object, xlim = start_tiles[i, 1:2], ylim = start_tiles[i, 
            3:4])
        dev.off()
    }
    ind <- createTilesRecur(object, start_coords, start_zoom, 
        title, max_zoom)
    if (ind == 1) 
        cat("Tiles created successfully\n")
    else cat("Error in creation of tiles!\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
