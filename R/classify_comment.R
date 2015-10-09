
#TODO implement continuation comments
#' @include is_parse_data.R

lint.comments <- data.frame(
      prefix = c( "#!", "#<", "#^", "#[")
    , class  = c( "LINT_COMMENT", "RELATIVE_COMMENT"
                , "CONTINUATION_COMMENT", "ATTRIBUTE_COMMENT")
    , stringsAsFactors = FALSE
    )

classify_comment <- function(comment){
    #! Classify Comments
    #! 
    #! Classify Comments according to the following rules:
    #! 
    #! * `#'` Roxygen Comment
    #! * `#!` Lint general comment
    #! * `#<` Lint relative placement comment
    #! * `#^` Lint continue previous comment.
    #! * `#[` Lint attribute comment.
    #! 
    #! 
    #! 
    #! 
    #! Attributes come in two flavors. `#[export]` is functionally equivalent
    #! to `#[export=TRUE]`
    if(is_parse_data(comment)){
        idx <- comment$token == "COMMENT"
        comment[idx, "token"] <- Recall(comment[idx, "text"])
        return(comment)
    }
    if(!is.character(comment)) return("")
    lead <- substring(comment, 1, 2)
    ifelse( nchar(comment)>0
       , ifelse( substring(comment, 1, 1) == "#"
           , ifelse( lead == "#'",  "ROXYGEN_COMMENT"
           , ifelse( lead == lint.comments[1, 1], lint.comments[1, 2]
           , ifelse( lead == lint.comments[2, 1], lint.comments[2, 2]
           , ifelse( lead == lint.comments[3, 1], lint.comments[3, 2]
           , ifelse( lead == lint.comments[4, 1], lint.comments[4, 2]
                   ,   "NORMAL_COMMENT"
                   )))))
           , "")
    , "")
}

is_lint <- function(comment #< a character vector of comments to test
){
    classify_comment(comment) %in% lint.comments$class
}
