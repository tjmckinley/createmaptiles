\name{LongLatToTileNum}
\alias{LongLatToTileNum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LongLatToTileNum(lon_deg, lat_deg, zoom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon_deg}{
%%     ~~Describe \code{lon_deg} here~~
}
  \item{lat_deg}{
%%     ~~Describe \code{lat_deg} here~~
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
function (lon_deg, lat_deg, zoom) 
{
    lat_rad <- lat_deg * pi/180
    n <- 2^zoom
    xtile <- n * ((lon_deg + 180)/360)
    ytile <- n * (1 - (log(tan(lat_rad) + (1/cos(lat_rad)))/pi))/2
    c(xtile, ytile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
