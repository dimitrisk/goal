
# ========================   Visualization   ========================================

#' @title vis.Hist
#' @description A nice histogram of a single variable with an additional Cumulative Density curve.
#'
#' @param inVector The vector of numbers to plot
#' @param ecdf Boolean for the addition of the red line of the Cumulative Density at the right of the histogram plot
#' @param ... Additional arguments to be passed to the 'hist' command of the base histogram.
#' @return A nice histogram of a single variable with an additional Cumulative Density curve.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords visualization
#' @family vis
#' @examples library(goal)
#'
#' set.seed(1821)
#' myNumbers = rnorm(500, 130, 10)
#' vis.Hist(myNumbers, freq=FALSE, main="This is the main title", xlab="X axis title")
#'
vis.Hist <- function(inVector, ecdf=T, ...){
  par(mar = c(5,5,2,5))
  h = hist(inVector,  ...)
  if(ecdf){
    par(new = T)
    ec = ecdf(inVector)
    plot(x = h$mids, y=ec(h$mids)*max(h$counts), col = "blue", axes = FALSE, xlab = NA, ylab = NA)
    lines(x = h$mids, y=ec(h$mids)*max(h$counts), col ='red', lwd=2)
    axis(4, at=seq(from = 0, to = max(h$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
    mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')
  }
}










#' @title vis.BarPlot
#' @description A nice barplot of a [table] with bar labels and absolute/relative frequencies.
#'
#' @param inTable The table to plot
#' @param percentage Whether to use percentages of absolute numbers
#' @param subtitle Whether to add an informative subtitle in the barplot
#' @param main The main title of the barplot
#' @param ylab The title in the `Y` axes
#' @param decimals Number of decimals to use
#' @return A nice barplot of the `inTable`
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @import plotrix graphics
#' @keywords visualization
#' @family vis
#' @examples library(goal)
#' library(plotrix)
#' cars = table( c(1,1,1,1,3,2,4,3,5,4,6,5,1,5,5,2,2,2,3,3,3,1,1,2) )
#' vis.BarPlot(inTable = cars, percentage = TRUE,  subtitle = TRUE,
#' main = "Relative frequency of cars", ylab = "Cars", decimals=1)
#'
vis.BarPlot <- function(inTable, percentage=F, subtitle=TRUE, main="main", ylab="", decimals=1){
  #library(plotrix)
  if (percentage){if (ylab == ""){ylab="Percentage"}}
  barpos <- graphics::barplot(inTable,  main = main, ylab=ylab,
                              names.arg = unlist(dimnames(inTable)), border="blue", density=c(80) )
  sub <- paste(length(inTable),"categories  | ",sum(inTable),"cases")
  if(subtitle){
    graphics::mtext(sub, side = 3, line = 0.3, cex = 0.9)
    }
  if (percentage) {
    labels <- inTable/(sum(inTable)/100)
    plotrix::boxed.labels(barpos, inTable / 2, ypad=1.5,
                          paste(format(round(labels, decimals),
                                       nsmall = decimals), "%", sep="")   )
  } else {
    labels <- inTable
    plotrix::boxed.labels(barpos, inTable / 2, ypad=1.5,
                          format(round(labels, decimals),
                                 nsmall = decimals))
  }
  graphics::box()
}
