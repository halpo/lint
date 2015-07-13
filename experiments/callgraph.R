
callgraph<-
graph_parse <- function(parse.data){
  stopifnot(require(Rgraphviz))
  d <- mutate( parse.data
             , id =as.character(id)
             , parent=as.character(parent)
             )
  g <- new("graphNEL", nodes=as.character(d$id), edgemode='directed')
  # bridge missing nodes for bug introduced sometime before 3.1.0
  # missing.nodes <- parse.data$parent[which(!with(parse.data, parent %in% id))]
  # if(length(missing.nodes)>1) 
    # for(i in 2:length(missing.nodes)){
      # current.id <- as.character(missing.nodes[[i]])
      # bridge.id <- as.character(missing.nodes[[i]] + 1L)
      # while(!(bridge.id %in% names(names(nodeData(g)))))
      # g <- addNode(bridge.id, g)
      # g <- addEdge( as.character(missing.nodes[[i]])
                  # , as.character(missing.nodes[[i]]+1L)
                  # , g, 1)
    # }
  for(i in seq.int(nrow(d))){
    if(d$parent[[i]] %in% d$id)
      g <- addEdge(d$id[[i]], d$parent[[i]], g, 1)
  }
  
  nAttrs <- list()
  nAttrs$label <- structure(ifelse(d$text!="", d$text, paste0("«", d$id, "»")), names = d$id)
  nAttrs$shape <- structure(mapToShapes(d$token), names=d$id)
  attrs <- list( node =list(shape="ellipse", fixedsize=FALSE)
               , graph=list(rankdir="BT")
               , edge =list(arrorhead='odiamond')
               )
  # nodeRenderInfo(g) <- 
  # list( shape = as.list(structure(mapToShapes(d$token), names=d$id))
      # , label = as.list(structure(ifelse(d$text!="", d$text, ""), names = d$id))
      # )
  # edgeRenderInfo(g) <- list(arrowhead = 'diamond')
  # renderGraph(layoutGraph(g))
  plot(g, "dot", nodeAttrs = nAttrs, attrs = attrs)
  invisible(g)
}

default.shape.mapping <- c(
       expr = "circle"
    )
mapToShapes <- function(token, mapping=default.shape.mapping, default.shape='box'){
  sh <- mapping[token]
  structure(ifelse(is.na(sh), default.shape, sh))
}

if(F){
    pdf("callgraph.pdf")
    graph_parse(
        fix_parent(parse.data)
    )
    dev.off()

    with(parse.data, parent %in% id)
    
    
}



