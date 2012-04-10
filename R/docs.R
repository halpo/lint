
get_docs <- function(parent.id, parse.data) {
#'  Find  all the documentation associated with an expression
#'  
#'  Rind all Roxygen documentation that is associated with an expression
#'  regardless of the location.  The traditional Roxygen documentation 
#'  must preceed a function declaration, but this allows for finding all 
#'  Roxygen comments regardless of the location.
#'  
#'  @param parent.id the id of the expression as defined by the 
#'    \code{parse.data}
#'  @param parse.data the results from \code{\link{parser}} for the file
#'    or text containg the expression of interest.
#'  
#'  @export
  if(length(parent.id)>1) alply(parent.id, get_docs, parse.data=parse.data)
  token.desc <- NULL
  kids <- get_child(parent.id, parse.data=parse.data, nlevels=-1L)
  parent.id <- c(parent.id, kids$id)
  parse.docs <- subset(parse.data,  
      parse.data$token.desc == "ROXYGEN_COMMENT"
    & (parse.data$parent %in% parent.id | parse.data$parent %in% -parent.id)
    )
}
