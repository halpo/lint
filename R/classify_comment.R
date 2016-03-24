#' @include is_parse_data.R

lint.comments <- data.frame(
      prefix = c( "#!", "#<", "#^")
    , class  = c( "LINT_COMMENT", "RELATIVE_COMMENT", "CONTINUATION_COMMENT")
    , stringsAsFactors = FALSE
    )

classify_comment <- function(comment){
    #! classify a comment as a type of comment
    if(is_parse_data(comment)){
        #! if passed a data.frame the token attribute is modified to 
        #! reflect the new comment.
        if(!is.null(lc <- attr(comment, "lint.classified")) && lc)
            return(comment)
        idx <- comment$token == "COMMENT"
        comment[idx, "token"] <- Recall(comment[idx, "text"])
        return(structure(comment, lint.classified=TRUE))
    }
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

is_lint_comment <- 
function(comment #< a character vector of comments to test
        ){
    classify_comment(comment) %in% lint.comments$class
}


get_comments <- function(parse.data){
    transform( subset(parse.data, token %in% c('COMMENT', 'ROXYGEN_COMMENT'))
             , lint.class = classify_comment(text))
}
make_get_lint_comments <- 
function( type = lint.comments$class  #< type of the comments to extract
        ){
    #! Create extraction functions for comment types.
    #! @keywords internal, utilities
    function(parse.data){
        x <- transform(parse.data, lint.class = classify_comment(text))
        subset(x, x$lint.class %in% type)
    }
}
get_lint_all_comments      <- make_get_lint_comments()
get_lint_relative          <- make_get_lint_comments("RELATIVE_COMMENT")
get_lint_continuation      <- make_get_lint_comments("CONTINUATION_COMMENT")
get_lint_comments          <- make_get_lint_comments("LINT_COMMENT")
get_lint_argument_descriptors <- 
make_get_lint_comments(c("RELATIVE_COMMENT", "CONTINUATION_COMMENT"))

get_associated_continuation <-
function( pd            #< parse data.
        , id=pd$id[1]   #< id of the comment of interest
        ){
    #! retrieve the continuation comments associated with the comment of interest.
    pd <- classify_comment(pd)
    start <- 
    end   <- which(pd$id==id)
    if(pd[start,'token'] %in%  c("LINT_COMMENT", "RELATIVE_COMMENT")){
        while(pd[end + 1, 'token'] == "CONTINUATION_COMMENT"){
            end <- end + 1
        }
        return(pd[start:end, ])
    } else if(pd[start, 'token'] %in% "ROXYGEN_COMMENT") {
        return(pd[start,])
    } else stop("not a valid starting comment.")
    #! filtered parse data with the comments, will be empty if the id does not denote a lint comment.
}


strip_lint_leads <- 
function( comment.text  #< The text of the comments.
        , space = TRUE  #< should the space at the beginning of the line be removed.
        ){
    #! Remove the characters identifying a lint comment.
    rx <- "^#[!^<{}]"
    if(space) rx <- paste0(rx, "\\s*")
    gsub(rx, "", comment.text)
}
