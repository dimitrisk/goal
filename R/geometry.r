

# ====================   Points Along Lines   ======================




#' Generate points along lines. Following a geometrical line, it produces points based on a factor.
#'
#' This function generates a number of points along lines.
#'
#' @title Points Along Lines
#' @param inroads A [SpatialLines()] or [SpatialLinesDataFrame()] object. The generated points will fall along this line.
#' @param thefactor A general multiplier factor which determines
#' the number of points to be generated along the line.
#' @return A [SpatialPointsDataFrame()] object which holds the generated points.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords geometry
#' @family geo
#' @import sp
#' @importFrom rgeos gLength
#' @examples library(goal)
#' library(sp)
#' library(rgeos)
#' x = c(1,2,2,1,1,1,3,3)
#' y = c(1,3,4,7,8,9,13,21)
#' thefactor=2
#' road = sp::SpatialLines(list(sp::Lines(Line(cbind(x,y)), ID="a")))
#'
#' result = goal::geo.PointsAlongLines(road, thefactor = thefactor)
#' plot(road)
#' plot(result, add=TRUE)
#' 
#' result2 = goal::geo.PointsAlongLines(road, thefactor = 1)
#' plot(road)
#' plot(result2, add=TRUE)
#'
geo.PointsAlongLines <- function(inroads, thefactor) {
  p <- spsample(inroads, rgeos::gLength(inroads) * thefactor, type = "regular")
  p <- sp::SpatialPointsDataFrame(p, data = data.frame(id = seq_len(length(p))))
  return(p)
}
