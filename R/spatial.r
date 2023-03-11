

# ====================   Spatial Anaysis   ======================




#' Generate points along lines. Following a geometrical line, it produces points based on a factor.
#'
#' Generates a number of points along lines.
#'
#' @title spa.MeanCenter
#' @param indf A n
#' @return A 
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords spatial_analysis point_pattern_analysis
#' @family spa
#' @examples library(goal)
#' co = data.frame(x = c(0,3.9,4.1,8), y = c(1,4.1,3.9,7))
#' names(co) = c("x","y")
#' (co)
#' 
#' goal::spa.MeanCenter(indf=co)
#' 
#'
spa.MeanCenter <- function(indf) {
  if(nrow(indf)<1){stop("This Data.Frame does not have rows")}
  if(! "x" %in% names(indf)){stop("There is no 'x' column.")}
  if(! "y" %in% names(indf)){stop("There is no 'y' column.")}
  return(c( mean(indf$x, na.rm=T), mean(indf$y, na.rm=T) ))
}
