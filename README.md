
# lab3algorithms

<!-- badges: start -->
[![R-CMD-check](https://github.com/njmurov-ux/lab3algorithms/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njmurov-ux/lab3algorithms/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package includes two functions: euclidean and dijkstra.
The euclidean function computes the greatest common divisor (GCD) of two integers using the Euclidean division method.
The dijkstra function determines the shortest distances from a specified starting node to all other nodes in a given DataFrame.

## Installation

You can install the development version of lab3algorithms from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("njmurov-ux/lab3algorithms")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lab3algorithms)
# Euclidean
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

# Dijkstra
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
```

