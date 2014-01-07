
#TODO implement continuation comments


lint.comments <- data.frame(
      prefix = c( "#!", "#<", "#^")
    , class  = c( "LINT_COMMENT", "RELATIVE_COMMENT", "CONTINUATION_COMMENT")
    , stringsAsFactors = FALSE
    )

classify_comment <- function(comment){
    if(!is.character(comment)) return("")
    lead <- substring(comment, 1, 2)
    ifelse( nchar(comment)>0
       , ifelse( substring(comment, 1, 1) == "#"
           , ifelse( lead == "#'",  "ROXYGEN_COMMENT"
           , ifelse( lead == lint.comments[1, 1], lint.comments[1, 2]
           , ifelse( lead == lint.comments[2, 1], lint.comments[2, 2]
           , ifelse( lead == lint.comments[3, 1], lint.comments[3, 2]
                   ,   "NORMAL_COMMENT"
                   ))))
           , "")
    , "")
}

is_lint <- function(comment #< a character vector of comments to test
){
    classify_comment(comment) %in% c("LINT_COMMENT", "RELATIVE_COMMENT")
}
