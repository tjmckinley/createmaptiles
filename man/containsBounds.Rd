\name{containsBounds}
\alias{containsBounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
containsBounds(tile, bounds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tile}{
%%     ~~Describe \code{tile} here~~
}
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
function (tile, bounds) 
{
    if (tile[2] < bounds[1]) 
        return(0)
    else {
        if (tile[1] > bounds[2]) 
            return(0)
        else {
            if (tile[4] < bounds[3]) 
                return(0)
            else {
                if (tile[3] > bounds[4]) 
                  return(0)
                else {
                  if (tile[1] < bounds[1] & tile[2] > bounds[2] & 
                    tile[3] < bounds[3] & tile[4] > bounds[4]) 
                    return(1)
                  else return(0.5)
                }
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
