#' Cut a hierarchical tree.
#'
#' This function groups close enough stocks into vertices (<= hc). It assign stocks between hc to hf to single-point-vertex (hc< h <= hf). It groups all stocks that are not closely correlated to each other into the last vertex (> hf).
#' This function returns a list, with first element being num_vertices (integer), second element points_in_vertex (a vector).
#' @param tree (return value of hclust()), hc (double, under which height to cluster trees), hf (double, above which height to ignore).
#' @return a list (integer + a list of vertex).
#' @export

cutree_manual <- function(tree, hc = NULL, hf = NULL){
  merge_mat <- tree$merge  # n-1 by 1 matrix, showing which point/result of which row are merged
  height <- tree$height # vector of n-1 elements, showing the height of a merged subtree
  if (is.null(n1 <- nrow(merge_mat)) || n1 < 1)
    stop("invalid tree!")
  if (is.null(hc) || is.null(hf))
    stop("under which height to cluster & above which height to ignore must be specified!")
  if (min(height) > hc)
    stop("no cluster formed, need to increase hc!")
  if (max(height) <= hc)
    stop("only one cluster formed, need to decrease hc!")
  
  step_hc <- which.max(height > hc) - 1
  step_hf <- ifelse(max(height) > hf, which.max(height > hf) - 1, n1)
  
  vertex_index <- 0
  points_in_vertex <- list()
  row_checkbox <- matrix(NA, nrow = step_hc, ncol = 2) # check if we have considered this row of points
  row_checkbox[, 1] <- 1:step_hc
  row_checkbox[, 2] <- TRUE
    
  for (i in step_hc:1){
    if (row_checkbox[i, 2]){
      vertex_index <- vertex_index + 1
      new_vertex <- create_vertex(merge_mat[1:(i+1), ], i) # need to load one more row, otherwise matrix with one row is converted to a vector.
      points_in_vertex[[vertex_index]] <- sort(new_vertex$points_contained)
      row_checkbox[new_vertex$rows_checked, 2] <- FALSE
    } 
  }
  
  for (i in (step_hc + 1):step_hf){
    if (merge_mat[i, 1] < 0){
      vertex_index <- vertex_index + 1
      points_in_vertex[[vertex_index]] <- abs(merge_mat[i, 1])
    }
    if (merge_mat[i, 2] < 0){
      vertex_index <- vertex_index + 1
      points_in_vertex[[vertex_index]] <- abs(merge_mat[i, 2])
    }
  }
  
  vertex_index <- vertex_index + 1
  uncorr_points <- c()
  for (i in (step_hf + 1):n1){
    if (merge_mat[i, 1] < 0){
      uncorr_points <- c(uncorr_points, abs(merge_mat[i, 1]))
    }
    if (merge_mat[i, 2] < 0){
      uncorr_points <- c(uncorr_points, abs(merge_mat[i, 2]))
    }
  }
  points_in_vertex[[vertex_index]] <- uncorr_points
  
  output <- list(num_vertices = vertex_index, points_in_vertex = points_in_vertex)
  return(output)
}
  
