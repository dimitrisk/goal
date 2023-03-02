---
title: "Similarity between vectors"
author: "Dimitris Kavroudakis"
date: "2022-12-13"
output: 
  rmarkdown::html_vignette:
    toc: true
    keep_md: true
    fig_caption: yes
    fig_width: 5
    fig_height: 5
  rmarkdown::pdf_document:
    toc: true
vignette: >
  %\VignetteIndexEntry{Similarity between vectors}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---

Aim

We need to measure the overall similarity between two vectors. This is the overall similarity between two groups of numbers. Additionally, we use a set of weights to indicate the importance of the elements in the two vectors. In the following example, each element has different importance. Similarity results, are sensitive to weights.

Download the goal library

```r
install.packages("http://www.dimitrisk.gr/Data/goal_0.1.tar.gz", repos=NULL, type="source")
```

Load library and create two vectors and the weights which reflect the importance of each element in vectors.

```r
library(goal)

weights=c(0.5, 0.25, 0.25)
final=c(42,5,36)
initial=c(40,5,36)
```

Now plot the two vectors.

```r
plot(final, initial, cex=weights*2, col="red",
  xlim=range(initial, final), ylim=range(initial, final))
abline(0,1)
```

![](/home/dimitrisk/working/code/REPOSITORY/goal/goal/vignettes/Similarity_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate the weighted similarity

```r
per = stats.SimilarityOfVectors(weights, final, initial)
print(per)
```

```
## [1] 0.9756098
```

The two vectors have 97.6 weighted similarity.
 
 
