#' Translate net information (R data frame) to JSON file.
#' This function takes all information of an igraph object and combine them.
#' @param  "net" (a data frame containing nodes and links information).  "file_loc" which is the output file with full path.
#' @return json format data.
#' @export

net_to_json <- function(net, file_loc = NULL){
  if (is.null(file_loc)){
    file_loc <- paste0(getwd(),"/data.json")
    file.create(file_loc)
  } else if (!file.exists(file_loc)){
    file.create(file_loc)
  }
  
  if (!is.data.frame(net$links) || !identical(colnames(net$links), c("source", "target", "value"))){
    stop("net$links should be a dataframe with 3 columns: source, target, value.")
  } else{
    net$links$source <- as.character(net$links$source)
    net$links$target <- as.character(net$links$target)
  }
  temp_links <- unname(split(net$links, seq(nrow(net$links)))) # split dataframe into lists
  temp_links <- lapply(temp_links, function(x) as.list(x)) # split list[1*3]/S3-dataframe further into lists
  
  if (!is.data.frame(net$nodes) || !identical(colnames(net$nodes), c("name", "group", "size", "comp"))){
    stop("net$nodes should be a dataframe with 4 columns: name, group, size, comp.")
  } else{
    colnames(net$nodes)[1] <- "id"
    net$nodes$id <- as.character(net$nodes$id)
    
    temp_nodes <- unname(split(net$nodes[, c(1,2,3)], seq(nrow(net$nodes[, c(1,2,3)])))) # split dataframe into lists
    temp_nodes <- lapply(temp_nodes, function(x) as.list(x)) # split list[1*3]/S3-dataframe further into lists
    
    for(i in seq_along(net$nodes$comp)){
      temp_pc <- data.frame(color = 1:11, percent = net$nodes$comp[[i]]) # 11 GICS sectors, model related
      temp_pc <- temp_pc[!(apply(temp_pc, 1, function(x) x[2] == 0)), ]
      temp_pc <- unname(split(temp_pc, seq(nrow(temp_pc))))
      temp_pc <- lapply(temp_pc, function(x) as.list(x))
      temp_nodes[[i]]<- c(temp_nodes[[i]], pieChart = list(temp_pc))
    }
  }
  
  temp_net <- list(nodes = temp_nodes, links = temp_links)
  data_j<-jsonlite::toJSON(temp_net, "rows", auto_unbox = T, pretty = T)
  write(data_j, file_loc)
}