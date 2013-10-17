\name{findMinZoom}
\alias{findMinZoom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findMinZoom(bounds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
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
function (bounds) 
{
    min_zoom <- sapply(as.list(1:18), function(x) {
        nrows <- 2^x
        xdiff <- 180 * 2/nrows
        ydiff <- (atan(sinh(pi)) * 2/nrows) * (180/pi)
        area_comp <- areaRect(bounds) - (xdiff * ydiff)
        if (area_comp > 0) 
            return(0)
        else return(1)
    })
    min_zoom <- which(min_zoom == 1)
    if (length(min_zoom) == 0) 
        return(0)
    else return(min_zoom[length(min_zoom)])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
