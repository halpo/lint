#' @import class-lintDocumentation.R
#' @export
documentedFunction <- 
setClass("documentedFunction", representation(doc = "functionDocumentation")
    , contains="function")
#TODO define show method

document_function <- function(
      FUN  #< Function to document
    , ...  #< Extra Documentation Arguments
    ){
    #! Document a funcion
    #! 
    #! This provides facilities to automatically document a function
    #! 
    fun <- as(FUN, 'documentedFunction')
    stopifnot(!is.null(srcref <- attr(FUN, 'srcref')))
    srcfile <- attr(srcref, 'srcfile')
    
    srclines <- as.character.srcref(srcref, TRUE)
    parse.data   <- getParseData(parse(text=srclines))
    
    comments <- 
    subset(parse.data, token %in% c('COMMENT'))
    
       
    fun@doc <- functionDocumentation(...)
    return(fun) #< an object of class `<documentedFunction>`
}
# document_function <- document_function(document_function)

#TODO: Program converting backticks to code arguments.
#TODO: Program converting angle brackets to link arguments.

inline_arguments <- function(parse.data #< `<data.frame>` from `<getParseData>`
){
    comments <- transform( subset(parse.data, token=='COMMENT')
             , lint.class = classify_comment(text)
             )
    relative.comments <- subset(comments, lint.class == "RELATIVE_COMMENT")
    
    argument.comments <- 
    relative.comments[is_argument(relative.comments$id, parse.data),]
    
    
    
    
}

is_argument <- function(id, parse.data)
{
    if(length(id)>1)
        return(sapply(id, is_argument, parse.data=parse.data))
    all_root_nodes(parse.data, id)

    parent <- get_parent(id, parse.data)
    parse.data[match(parents, parse.data$id), ]

    family <- get_family(id, parse.data, 1, 0)
    family[1,'token'] == 'FUNCTION'
}

get_argument

