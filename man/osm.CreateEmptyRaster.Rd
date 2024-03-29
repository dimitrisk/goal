% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osm.r
\name{osm.CreateEmptyRaster}
\alias{osm.CreateEmptyRaster}
\title{osm.CreateEmptyRaster}
\usage{
osm.CreateEmptyRaster(inPerioxi = "Mytilene Municipal Unit")
}
\arguments{
\item{inPerioxi}{The name of an area (string) for searching the OSM services.}
}
\value{
An empty raster that covers the full area
}
\description{
Create an empty raster for an area. It downloads the area from OSM services, and then creates an empty raster of this area.
}
\examples{
library(devtools)
#install_github("dimitrisk/goal")
library(raster)
library(goal)
result = goal::osm.CreateEmptyRaster(inPerioxi="Mytilene Municipal Unit")
result
}
\seealso{
Other osm: 
\code{\link{osm.ClipSFnetwork_with_poly}()},
\code{\link{osm.bb_2_pol}()},
\code{\link{osm.combineAmenities}()},
\code{\link{osm.combineShops}()},
\code{\link{osm.getFrequency}()},
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
\keyword{openstreetmap,}
\keyword{raster}
