% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osm.r
\name{osm.getFrequency}
\alias{osm.getFrequency}
\title{osm.getFrequency}
\usage{
osm.getFrequency(indf, inword = "shop", removeNA = T)
}
\arguments{
\item{indf}{The vec}

\item{inword}{Boole}

\item{removeNA}{Ad}
}
\value{
A nice histogram of a single variable with an additional Cumulative Density curve.
}
\description{
Get Frequency Table
}
\examples{
library(goal)
sh = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "shop")
shops = osm.combineShops(sh)
freq2 = osm.getFrequency(shops, inword = "shop", removeNA = F)
freq2
}
\seealso{
Other osm: 
\code{\link{osm.ClipSFnetwork_with_poly}()},
\code{\link{osm.CreateEmptyRaster}()},
\code{\link{osm.bb_2_pol}()},
\code{\link{osm.combineAmenities}()},
\code{\link{osm.combineShops}()},
\code{\link{osm.getLength_footway}()},
\code{\link{osm.getPOI_usingbb}()},
\code{\link{osm.getPOI}()},
\code{\link{osm.getRoads}()},
\code{\link{osm.osmdata_result_2_bbox_pol}()}
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}
\concept{osm}
\keyword{POIs}
\keyword{frequency,}
\keyword{openstreetmap,}
