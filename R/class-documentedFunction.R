{###############################################################################
# class-DocumentedFunction.R
# Andrew Redd
# 2013-10-06
# 
# DESCRIPTION
# ===========
# functions for self documenting functions
# 
}###############################################################################
#' @include class-lintDocumentation.R
#' @include classify_comment.R

#' @export
documentedFunction <- 
setClass("documentedFunction", representation(doc = "functionDocumentation")
    , contains="function")
#TODO define show method

document_function <- function(
      FUN  #< Function to document
    , ...  #< Extra Documentation Arguments
    ){
    #! Document a function
    #! 
    #! This provides facilities to automatically document a function
    #! 
    force(FUN)
    stopifnot(!is.null(srcref <- attr(FUN, 'srcref')))
    srcfile <- attr(srcref, 'srcfile')
    
    srclines <- as.character.srcref(srcref, TRUE)
    function.parse   <- getParseData(parsed <- parse(text=srclines))
    
    fun <- as(FUN, 'documentedFunction')
    fun@doc <- functionDocumentation(args = document_arguments(function.parse))
    return(fun) #< an object of class `<documentedFunction>`
}

get_comments <- function(parse.data){
    transform( subset(parse.data, token %in% c('COMMENT', 'ROXYGEN_COMMENT'))
             , lint.class = classify_comment(text))
}
make_get_lint_comments <- function(type = lint.comments$class){
    #TODO turn into a self documented function that carries forward documentation. 
    function(parse.data){
        x <- transform(parse.data, lint.class = classify_comment(text))
        subset(x, x$lint.class %in% type)
    }
}
get_lint_all_comments      <- make_get_lint_comments()
get_lint_relative      <- make_get_lint_comments("RELATIVE_COMMENT")
get_lint_continuation  <- make_get_lint_comments("CONTINUATION_COMMENT")
get_lint_comments      <- make_get_lint_comments("LINT_COMMENT")
get_lint_argument_descriptors <- 
make_get_lint_comments(c("RELATIVE_COMMENT", "CONTINUATION_COMMENT"))
get_function_body_parse <- function(function.parse){
    body.parent <- tail(subset(function.parse, parent == id[1])$id, 1)
    get_family(body.parent, function.parse)
}
get_previous_expr <- function(id, parse.data){
    stopifnot(length(id) ==1)
    this.id <- id
    parent <- subset(parse.data, id == this.id)
    level.data  <- 
    get_family(id, parse.data, 1, 0)

}



strip_lint_leads <- function(comment.text){
    gsub("^#[!^<{}]\\s*", "", comment.text)
}

# title      
# description
{# Arguments
document_arguments <- function(function.parse #< `<data.frame>` from `<getParseData>` for a single function.
){
    #! Document Arguments from a parse.data
    comments <- get_comments(function.parse)
    relative.comments <- subset(comments, lint.class == "RELATIVE_COMMENT")
    
    argument.comments <- 
    relative.comments[is_argument(relative.comments$id, function.parse),]
    
    get_arguments(argument.comments, function.parse)
}
is_argument <- function(id, parse.data){
    #! Test if an element of parsed data is an argument relative comment.
    if(length(id)==0)return(logical(0))
    if(length(id)>1)
        return(sapply(id, is_argument, parse.data=parse.data))
    all_root_nodes(parse.data, id)

    parent <- get_parent(id, parse.data)
    parse.data[match(parent, parse.data$id), ]

    family <- get_family(id, parse.data, 1, 0)
    family[1,'token'] == 'FUNCTION'
}
get_arguments <- function( argument.comments #< `data.frame` of parse data that contain the relevant relative argument comments.
                         , function.parse    #< the full parse data for the entire function
){
    #! get associated argument for argument relative comments
    #TODO: Vectorize
    argument.parse <- only_arguments(function.parse)
    args.splits    <- split_arguments(argument.parse, root=function.parse[1, 'id'])
   
   
    # Test for the integrity of the returned list.
    l_ply(args.comments <- llply(args.splits, get_comments), function(pd){
        if(sum(pd$lint.class == "RELATIVE_COMMENT")>2)
            stop("Only one relative line per argument is allowed.")
    })
    
    arguments <- lapply(args.splits, form_argument)
    argList(.list=arguments)
}
only_arguments <- function(function.parse #< parse information for only one.function.  
                                          #^ The first row must be the root node for the function.
                          , root = function.parse$id[[1]]
){
    #! Return only the argument portion of the parse data.
    argument.parse <- subset(function.parse, parent == id[1])
    stopifnot( head(argument.parse,1)$token == "FUNCTION"
             , tail(argument.parse,1)$token == 'expr'
             )
    argument.parse <- tail(head(argument.parse, -2), -2)
    get_child(argument.parse$id, function.parse)
}
split_arguments <- function( argument.parse #< argument only parse data.
                           , root = max(argument.parse$parent)
){
    #! split argument.parse data into list of argument information.
    list.args.parse <- split(argument.parse
                            , cumsum( argument.parse$token  == "','" 
                                    & argument.parse$parent == root  )
                            )
    list.args.parse <- lapply( list.args.parse, subset, token != "','")
    return(list.args.parse) #< returns a list with parse data for each argument.
}

{#! Form Argument from parse data
form_argument <- function(split.parse #< parse data for a single argument.
){
    #! Form a `<argumentDocumentation>` from parse data information.
    arg( name        = find_arg_name(split.parse)
       , default     = find_arg_default(split.parse)
       , description = find_arg_description(split.parse)
       )
}
find_arg_name <- function(split.parse){
    split.parse[1,'text']
}
find_arg_default <- function(split.parse){
    expr.parse <- 
    get_child( id = tail(split.parse[!is_comment(split.parse), ], 1)$id
             , split.parse
             , include.parent = FALSE
             , all.generations = TRUE
             )
    reconstitute_expression(expr.parse)
}
reconstitute_expression <- function(expr.parse, id = expr.parse$id){
    parse(text = getParseText(expr.parse, id=id))
}
find_arg_description <- function(split.parse){
    comment.text <- get_lint_argument_descriptors(split.parse)$text
    paste(strip_lint_leads(comment.text), collapse=' ')
}
}
}
# seealso    
{# return
# Relative return
get_return_relative <- function(function.parse){
    parse.data <- 
    body.parse <- get_function_body_parse(function.parse)
    relative.in.body <- get_lint_relative(body.parse)
    id <- relative.in.body$id
}



# @return tag

}

