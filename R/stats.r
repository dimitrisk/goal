





#' Similarity between vectors
#'
#' A weighted similarity measure between two numeric vectors.
#'
#' @param weights Vector of weights for each observation.
#' @param final Vector of numbers.
#' @param initial Vector of numbers.
#'
#' @return A percentage of similarity between the two numeric vectors.
#' @export
#'
#' @examples library(goal)
#'
#' weights=c(0.5, 0.25, 0.25)
#' final=c(42,5,36)
#' initial=c(40,5,36)
#'
#' plot(final, initial, cex=weights*2, col="red",
#'   xlim=range(initial, final), ylim=range(initial, final))
#' abline(0,1)
#'
#' per = goal::stats.SimilarityOfVectors(weights, final, initial)
#' print(per)
stats.SimilarityOfVectors <- function(weights, final, initial) {
  if (length(weights) != length(final) | length(weights) != length(initial)){
    stop("'weights' not the same length as: 'final' or 'initial'")
  }
  if (length(initial) != length(final)){
    stop("'initial' not the same length as 'final'  ")
  }

  if (sum(weights) != 1 ){
    stop("'weights' should sum up to 1. For example: c(0.5, 0.25, 0.25) ")
  }
  temp <- weights * (final/initial)
  synolo <- sum(temp, na.rm = T)
  result <- 1- abs(1-(1/synolo))
  return(result)
}




