
#TODO implement continuation comments
classify_comment <- function(comment){
    if(!is.character(comment)) return("")
    lead <- substring(comment, 1, 2)
    ifelse( nchar(comment)>0
       , ifelse( substring(comment, 1, 1) == "#"
           , ifelse( lead == "#'",  "ROXYGEN_COMMENT"
           , ifelse( lead == "#!",     "LINT_COMMENT"
           , ifelse( lead == "#<", "RELATIVE_COMMENT"
           , ifelse( lead == "#^", "CONTINUATION_COMMENT"
                                 ,   "NORMAL_COMMENT" 
                                 ))))
           , "")
    , "")
}

is_lint <- function(comment #< a character vector of comments to test
){
    classify_comment(comment) %in% c("LINT_COMMENT", "RELATIVE_COMMENT")
}
