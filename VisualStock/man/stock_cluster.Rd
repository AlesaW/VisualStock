% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stock_cluster.R
\name{stock_cluster}
\alias{stock_cluster}
\title{Cluster stocks}
\usage{
stock_cluster(distance_matrix, hc = NULL, hf = NULL)
}
\arguments{
\item{distance_matrix, }{hc (double, under which height to cluster trees, user-specified or automatically generated to cut 1/3 points of the tree), hf (double, above which height to ignore, user specified or automatically generated to be the mid-point of the first empty bin after the bin of hc).}
}
\value{
a list (adjacency matrix, num_vertices, points_in_vertex(list)).
}
\description{
This function groups close enough stocks into vertices (<= hc). It assign stocks between hc to hf to single-point-vertex (hc< h <= hf). It groups all stocks that are not closely correlated to each other into one vertex (> hf). The above is realized by calling function cutree_manual().
The function then calculate the distance (min) between vertices and create a 2-level weighted adjacency matrix between vertices. Distance within (0, 0.77] is assigned weight 2, distance within (0.77, 0.89] is assigned weight 1, distance larger than hf is assigned weight 0.
}
