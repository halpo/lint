

graph_parse <- function(parse.data){
  stopifnot(require(Rgraphviz))
  d <- mutate(parse.data,  id =as.character(id), parent=as.character(parent))
  g <- new("graphNEL", nodes=as.character(d$id), edgemode='directed')
  for(i in seq.int(nrow(d))){
    if(d$parent[[i]] %in% d$id)
      g <<- addEdge(id[[i]], parent[[i]], g, 1)
  }
  
  
  nAttrs <- list()
  nAttrs$label <- structure(ifelse(d$text!="", d$text, ""), names = d$id)
  nAttrs$shape <- structure(mapToShapes(d$token), names=d$id)
  attrs <- list( node=list(shape="ellipse", fixedsize=FALSE)
               , graph=list(rankdir="BT")
               , edge =list(arrorhead='odiamond')
               )
  plot(g, "dot", nodeAttrs = nAttrs, attrs = attrs)
  
  
  
  nodeRenderInfo(g) <- 
  list( shape = as.list(structure(mapToShapes(d$token), names=d$id))
      , label = as.list(structure(ifelse(d$text!="", d$text, ""), names = d$id))
      )
  edgeRenderInfo(g) <- list(arrowhead = 'diamond')
  renderGraph(layoutGraph(g))
}
 
unique(d$token.desc)
 
default.shape.mapping <- c(
       expr = "circle"
    )
mapToShapes <- function(token, mapping=default.shape.mapping, default.shape='box'){
  sh <- mapping[token]
  structure(ifelse(is.na(sh), default.shape, sh))
}






