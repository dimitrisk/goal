## ---- eval=F,   message=F-----------------------------------------------------
#  install.packages("http://www.dimitrisk.gr/Data/goal_0.1.tar.gz", repos=NULL, type="source")

## ----   message=F-------------------------------------------------------------
library(goal)

weights=c(0.5, 0.25, 0.25)
final=c(42,5,36)
initial=c(40,5,36)

## ----   message=F-------------------------------------------------------------
plot(final, initial, cex=weights*2, col="red",
  xlim=range(initial, final), ylim=range(initial, final))
abline(0,1)

## ----   message=F-------------------------------------------------------------
per = stats.SimilarityOfVectors(weights, final, initial)
print(per)

