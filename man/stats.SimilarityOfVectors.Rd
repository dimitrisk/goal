% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.r
\name{stats.SimilarityOfVectors}
\alias{stats.SimilarityOfVectors}
\title{Similarity between vectors}
\usage{
stats.SimilarityOfVectors(weights, final, initial)
}
\arguments{
\item{weights}{Vector of weights for each observation.}

\item{final}{Vector of numbers.}

\item{initial}{Vector of numbers.}
}
\value{
A percentage of similarity between the two numeric vectors.
}
\description{
A weighted similarity measure between two numeric vectors.
}
\examples{
library(goal)

weights=c(0.5, 0.25, 0.25)
final=c(42,5,36)
initial=c(40,5,36)

plot(final, initial, cex=weights*2, col="red",
  xlim=range(initial, final), ylim=range(initial, final))
abline(0,1)

per = goal::stats.SimilarityOfVectors(weights, final, initial)
print(per)
}
