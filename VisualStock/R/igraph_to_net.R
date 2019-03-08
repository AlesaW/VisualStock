#' Generate net information.
#' This function takes all information of an igraph object and combine them.
#' @param "g" (an igraph object), "name" of vertices (vector), "group" (a vector of integers range from 1 to N), "size" of vertices (vector), "comp"_onents of the piechart (a list of vectors).
#' @return a data frame of either nodes or links or a list of both data frames.
#' @export
igraph_to_net <- function (g, name = NULL, group = NULL, size = NULL, comp = NULL, what = "both") 
{
  if (!("igraph" %in% class(g))) 
    stop("g must be an igraph class object.")
  if (!(what %in% c("both", "links", "nodes"))) 
    stop("what must be either \"nodes\", \"links\", or \"both\".", 
         call. = FALSE)
  
  temp_nodes <- data.frame(id = as_ids(V(g)))
  if (!is.null(name)){
    temp_nodes$name <- name
    nodes <- data.frame(name = name)
  } else{
    temp_nodes$name <- temp_nodes$id
    nodes <- data.frame(name = temp_nodes$id)
  }
  
  if (is.null(group))
    stop("must assign each node a group number for color use.")
  if (nrow(nodes) != length(group))
    stop("each node can have one and only one group number.")
  nodes$group <- group
  
  if (!is.null(size))
    nodes$size <- size
  else
    nodes$size <- rep(1, nrow(nodes)) # default to same node size
  
  if (!is.null(comp)){
    temp_comp <- as.data.frame(comp, col.names = 1:nrow(nodes))
    pct_comp <- lapply(temp_comp, function(x) x/sum(x)*100)
  } else{
    pct_comp <- rep(list(rep(0, 11)), nrow(nodes)) # 11 GICS sectors, model related
    for (i in 1:length(temp_comp))
    {
      pct_comp[[i]][group[i]] <- 100  # default to be the group color
    }
  }
  nodes$comp <- pct_comp
    
  links <- as_data_frame(g, what = "edges")
  links <- merge(links, temp_nodes, by.x = "from", by.y = "id")
  links <- merge(links, temp_nodes, by.x = "to", by.y = "id")
  links <- links[, c(4, 5, 3)] %>% setNames(c("source", "target", "value"))
 
  if (what == "both") {
    return(list(links = links, nodes = nodes))
  }
  else if (what == "links") {
    return(links)
  }
  else if (what == "nodes") {
    return(nodes)
  }
}