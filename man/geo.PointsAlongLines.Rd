% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.r
\name{geo.PointsAlongLines}
\alias{geo.PointsAlongLines}
\title{Points Along Lines}
\usage{
geo.PointsAlongLines(inroads, thefactor)
}
\arguments{
\item{inroads}{A \code{\link[=SpatialLines]{SpatialLines()}} or \code{\link[=SpatialLinesDataFrame]{SpatialLinesDataFrame()}} object. The generated points will fall along this line.}

\item{thefactor}{A general multiplier factor which determines
the number of points to be generated along the line.}
}
\value{
A \code{\link[=SpatialPointsDataFrame]{SpatialPointsDataFrame()}} object which holds the generated points.
}
\description{
Generate points along lines. Following a geometrical line, it produces points based on a factor.
}
\details{
This function generates a number of points along lines.
}
\examples{
library(goal)
library(sp)
library(rgeos)
x = c(1,2,2,1,1,1,3,3)
y = c(1,3,4,7,8,9,13,21)
thefactor=2
road = sp::SpatialLines(list(sp::Lines(Line(cbind(x,y)), ID="a")))

result = goal::geo.PointsAlongLines(road, thefactor = thefactor)
plot(road)
plot(result, add=TRUE)

result2 = goal::geo.PointsAlongLines(road, thefactor = 1)
plot(road)
plot(result2, add=TRUE)

}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}
\concept{geo}
\keyword{geometry}
