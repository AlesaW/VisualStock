#' Cluster stocks
#'
#' This function groups close enough stocks into vertices (<= hc). It assign stocks between hc to hf to single-point-vertex (hc< h <= hf). It groups all stocks that are not closely correlated to each other into one vertex (> hf). The above is realized by calling function cutree_manual().
#' The function then calculate the distance (min) between vertices and create a 2-level weighted adjacency matrix between vertices. Distance within (0, 0.77] is assigned weight 2, distance within (0.77, 0.89] is assigned weight 1, distance larger than hf is assigned weight 0.
#' @param distance_matrix, hc (double, under which height to cluster trees, user-specified or automatically generated to cut 1/3 points of the tree), hf (double, above which height to ignore, user specified or automatically generated to be the mid-point of the first empty bin after the bin of hc).
#' @return a list (adjacency matrix, num_vertices, points_in_vertex(list)).
#' @export

stock_cluster <- function(distance_matrix, hc = NULL, hf = NULL){
  tree <- fastcluster::hclust(as.dist(distance_matrix), method = "single")
  if (is.null(n1 <- nrow(tree$merge)) || n1 < 1)
    stop("invalid tree!")
  if (is.null(hc))
    params <- find_params(tree)
    hc <- params$hc
  if (is.null(hf))
    hf <- params$hf
  
  cluster <- cutree_manual(tree, hc, hf)
  num_vertices <- cluster$num_vertices
  points_in_vertex <- cluster$points_in_vertex
  adja_mat <- mat.or.vec(num_vertices, num_vertices) # default to be a zero matrix.
  for (i in 1:(num_vertices-1)){ # note last vertex is not connect to anyone.
    for (j in (i + 1):num_vertices){
      d <- min(distance_matrix[points_in_vertex[[i]], points_in_vertex[[j]]])
      if (d <= sqrt(2*(1-0.7))){
        adja_mat[i, j] <- 2
      } else if (d > sqrt(2*(1-0.7)) && d <= sqrt(2*(1-0.6))){
        adja_mat[i, j] <- 1
      }
      adja_mat[j, i] <- adja_mat[i, j]
    }
  }
  output <- list(adjacency = adja_mat, num_vertices = num_vertices, points_in_vertex = points_in_vertex)
  return(output)
}
