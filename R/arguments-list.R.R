{#######################################################################
# arguments-list.R
# Andrew Redd
# (c) 2015-11-04
#
# DESCRIPTION
# ===========
# arguments-list class
# 
}#######################################################################

is_pd_arg_rel_com <- 
function( pd          #< A [parse-data] object.
        , id = pd$id  #< [integer] Id(s) to check.
        ){
    #! Test if an element of parsed data is an argument relative comment.
    if(length(id)==0)return(logical(0))
    if(length(id)>1)
        return(sapply(id, pd_is_argument_relative_comment, pd=pd))
    all_root_nodes(pd, id)

    parent <- get_parent(id, pd)
    pd[match(parent, pd$id), ]

    family <- get_family(id, pd, 1, 0)
    family[1,'token'] == 'FUNCTION'
    #! @return logical of length `length(id)` each indicating if the exlement 
    #! is a relative argument comment. 
}

arguments_list_from_pd <- 
function( pd #< The [parse-data]
        , id = all_root_ids(pd) #< id indicating the function of interest. 
        ){
    #! Document Arguments from a `<parse-data>` data object.
    if(is_pd_assignment(pd))
        pd <- get_pd_assign_value(pd)
    stopifnot(is_pd_function(pd))
    args.pd <- split_arguments(only_arguments(pd))
    
    args.info <- lapply(args.pd, arg_from_pd)
    for(i in seq_along(args.info)){
        names(args.info)[i] <- args.info[[i]][['name']]
    }
    structure( args.info
             , class=c("arguments-list", "lint-documentation")
             )
}
if(F){# Development
    pd <- get_parse_data(arg)
}

get_arguments <- 
function( pd    #< The [parse-data]
        , id = all_root_ids(pd) #< id identifying the function(s) of interest.
        ){
    #! get associated argument for argument relative comments
    stopifnot(length(id)==1)    
    if(length(id)>1)
        lapply(id, get_arguments, pd=pd)
    
    args.pd <- split_arguments(only_arguments(pd=pd, id=id))
   
    
    # Test for the integrity of the returned list.
    l_ply(args.comments <- llply(args.splits, get_comments), function(pd){
        if(sum(pd$lint.class == "RELATIVE_COMMENT")>2)
            stop("Only one relative line per argument is allowed.")
    })
    
    arguments <- lapply(args.splits, form_argument)
    argList(.list=arguments)
}

only_arguments <- 
function( pd #< `parse-data` for only one.function 
        , id = all_root_nodes(pd)$id #< id of the function root expression.
        ){
    #! Return only the argument portion of the parse data.
    args.pd <- pd[pd$parent == id,]
    stopifnot( head(args.pd,1)$token == "FUNCTION"
             , tail(args.pd,1)$token == 'expr'
             )
    args.pd <- tail(head(args.pd, -2), -2)
    get_child(args.pd$id, pd=pd)
}
split_arguments <- 
function( pd #< argument only parse data.
        ){
    #! split argument parse-data, `pd`, into list of argument information.
    list.args.parse <- split( pd
                            , cumsum( pd$token  == "','" )
                            )
    list.args.parse <- lapply( list.args.parse, subset, token != "','")
    return(list.args.parse) #< returns a list with parse data for each argument.
}
if(F){
    
    
    
}


arguments_list <- 
function( ... #< Argument information, see details.
        , .list = list(...) #< for passing argumemnts as a [list].
        , parse.strings = FALSE #< Should strings be parsed for extra information?
        ){
    #! Create an `arguments-list` container for function arguments
    #!
    #! The `arguments-list` class contains the information for arguments
    #! for a function.  The structure is internally a named homogeneous list 
    #! with each element an object of `<argument-documentation>` 
    #! which is created by the `<arg>` function.
    #! The class inherits from the S3 virtual class lint-documentation.
    #! 
    #! 
    #! @details
    #! arguments may be passed to argumentsList in three forms.
    #! 
    if(!missing(.list) && length(...))
        stop("may not specify both `...` and `.list`")
    if(length(.list)==0)
        return(structure(list(), class=c("arguments-list", "lint-documentation")))
    
    #! @subsetion `name="description"`
    #! The coder may specify argument information by `name = "description" where the
    #! provided name with be taken as the argument name and description through
    #! `<arg>`. See `<arg>` for how strings can be structured.
    #! 
    is.arg   <- sapply(.list, inherits, "argument-info")
    has.name <- names(.list) != "" & !is.arg
    for(i in which(has.name)){
        if( parse.strings )
            .list[[i]] <- 
                arg_from_string(as.character(.list[[i]]), name = names(.list)[i] )
        else
            .list[[i]] <- 
                arg( name        = names(.list)[i]
                   , description = as.character(.list[[i]])
                   )
    }
    
    #! @subsection As `<parse-data>`
    #! Passing a dataframe
    #! 
    if(length(.list)==1 && is_parse_data(pd<-.list[[1]])){
        .list <- arguments_list_from_pd(pd)
    }
    
    #! @subsection as an `argument-documentation`
    #! You may specify an argument as a `<argument-documentation>`
    #! created from the `<arg>` function.  In this case, 
    #! the object will be taken as is, ignoring any names specified.
    #! 
    is.arg <- sapply(.list, inherits, "argument-info")
    if(!all(is.arg)) warning("There were arguments to `arguments_list` that could not be converted to `argument-info`.")
    names(.list) <- sapply(.list, getElement, "name")
    
    structure(.list, class=c("arguments-list", "lint-documentation", "list"))
}
if(F){# Development
    debug(arguments_list)
    
    pd <- get_parse_data(pd)
    get_pd_assign_value(pd)
    
    only_arguments(pd)
    
}
if(F){#!@testthat arguments_list
    pd <- get_parse_data(arguments_list)
    if(is_pd_assignment(pd))
    al <- arguments_list(pd)
    expect_is(al, "arguments-list")
    expect_is(al, "lint-documentation")
    expect_equal(length(al), 3)
    expect_true(all(sapply(al, inherits, "argument-info")))
    
    al <- arguments_list( a = "a letter"
                        , b = "a [numeric]"
                        , c = "[function] the concatenation function"
                        , parse.strings=TRUE
                        )
    expect_is(al, "arguments-list")
    expect_is(al, "lint-documentation")
    expect_equal(length(al), 3)
    expect_equal(names(al), c('a', 'b', 'c'))
    expect_true(all(sapply(al, inherits, "argument-info")))
    
}

`format_md.arguments-list` <- 
function( x         #< The [arguments-list] object
        , indent=0  #< indent level
        , ...       #< Passed on.
        , header = "# Arguments"  #< header for section
        , collapse = FALSE #< collapse the lines?
        ){
    #! format for markdown.
    #![[export]]
    lines <- c( header
              , paste0( sapply(x, `format_md.argument-info`, indent=indent+1)
                      , collapse=if(collapse){ "\n" } )
              )
    if(!collapse) return(lines)
    return(collapse(lines))
}
`format_Rd.arguments-list` <- 
function( x         #< The [arguments-list] object
        , ...       #< Passed on.
        , collapse = FALSE #< [logical] collapse the section into single string(TRUE) 
                           #^ or leave as lines(FALSE).
        ){
    #! format for markdown.
    #![[export]]
    arglines <- unname(sapply(x, `format_Rd.argument-info`))
    lines <- c("\\arguments{", arglines, "}")
    if(!collapse) return(lines)
    return(paste(lines, collapse='\n'))
}
if(F){#!@testthat
al <- arguments_list('...'="anything")
expected <- c( "\\arguments{"
             , "\\item{...}{anything}"
             , "}")
expect_equal(format_Rd(al), expected)

al <- arguments_list(a="something", '...'="anything")
expected <- "\\arguments{
\\item{a}{something}
\\item{...}{anything}
}"
expect_equal(format_Rd(al, collapse=TRUE), expected)
}

`print.arguments-list` <- 
function( x
		, ...
		, format = c("md", "Rd")
		){
	#! Print a arguments-list object.
	format <- match.arg(format)
	formater <- match.fun(paste0("format_", format, ".", class(x)[[1]]))
	cat(formater(x, ...), "\n")
}


