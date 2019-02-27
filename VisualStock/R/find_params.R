#' Automatically produce hc and hf
#' 
#' This function automatically generate parameters to cut the hierachical tree. 
#' hc: the height under which there are 1/3 points. create a histogram, find the mid point of the bin that left bins of this bin contains 1/3 of the points.
#' hf: the mid point of the bin that is the first empty bin on the right of the hc bin in the histogram after.
#' @param tree (return value of hclust())
#' @return a list of hc and hf
#' @export

find_params <- function(tree){
  height <- tree$height
  total_cnts <- length(height) + 1
  bin_breaks <- seq(min((height)), max(height), by = (max(height) - min(height))/total_cnts)
  myhist <- hist(height, breaks = bin_breaks, plot = FALSE)
  cum_counts <- cumsum(myhist$counts)
  pos_hc <- which.max(cum_counts>total_cnts/3)
  hc <- myhist$mids[pos_hc]
  pos_hf <- which.max(myhist$counts[pos_hc:total_cnts] == 0) + pos_hc - 1
  hf <- myhist$mids[pos_hf]
  output <- list(hc = hc, hf = hf)
  return(output)
}
