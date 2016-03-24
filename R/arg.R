########################################################################
# args.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Functions for creating arguments for function documentation.
#
########################################################################
arg <- 
function( name = deparse(substitute(x))
        , description = ""	#< Description string
		, classes           #< [character] expected classes, missing implies any class.
		, default			#< [character|expression] representing the default.
		, x 				#< [expression] Object to retrieve information from
		, ...               #< included in the object to provide forward compatability.
        ){
    #! Create an `argument-info` object
    if(!missing(x) && missing(default) && missing(class) && missing(info)){
        if(inherits(x, 'argument-info'))return(x)
    }
    default <-
        if(!missing(default)) default else
        if(!missing(x)) find_default(x) else
        .no.default
    classes <-
        if(!missing(classes)){
            if(length(classes)==1 && tolower(classes) == "any")
                structure("ANY", any.class=TRUE)
            else classes
        } else if(!missing(x) ){
            if(inherits(default, "no-default-arg")
               || base::class(x)[[1]] == "name")
                structure("UNKNOWN", unknown.classes=TRUE)
            else
                base::class(x)[[1]]
        } else {
            structure("ANY", any.class=TRUE)
        }

    #! @alias class-argument-info argument-info
	#! @section argument-info class.
	#! The argument-info class encapsulates the information for a function argument.
	#! It extends `<list>` and `<lint-documentation>` classes.
	#! It must contain a description element and may contain:
	#! 
	#! * `default` which describes the default for the argument.
	#! * `class` which describes acceptable classes to be passed in.
	#! 
	#! @section Specifying in arguments.
	#! Argument information may be documented in text with adding `#<` 
	#! to the same line as the argument.  In the case of multiple arguments present 
	#! on the same line the last argument will be documented. 
	#! 
	#! The default will be infered from the function definition.
	#! The class element may be specified by wrapping square brackets `[]`
	#! around words.  Multiple classes may be specified in in the brackets 
    #! by separating with a pipe `|`.
    #! If specified first it will be ommitted if inside the 
	#! remaining text will be left.  All the remaining text is the description.
	#! 
    if(!inherits(description, "character"))
        stop("invalid `description`: expected a character received a ", class(description))
    if(length(description)>1)
        description <- paste(description, collapse=' ')
	structure(list( name        = name
                  , description = description
                  , class       = classes
                  , default     = default
                  , ...
                  )
             , class = c("argument-info", "lint-documentation", "list")
             )
	#! @return An object of S3 class `argument-info`.
    #! @export
}
if(FALSE){#! @TESTTHAT arg
    test_that("lint::arg, basic", {
        x <- arg("name", "description", "class", 'default')
        expect_true(all(c("name", "description", "class", 'default') %in% names(a)))
        expect_equal(x$name, "name")
        expect_equal(x$description, "description")
        expect_equal(x$class, "class")
        expect_equal(x$default, "default")
    })
    test_that("lint::arg, called directly.", {
        a <- arg("a number", class="numeric", default=1)
        expect_is(a, "argument-info")
        expect_equal(a@info, "a number")
        expect_equal(a@default, 1)
        expect_equal(a@classes, "numeric")
    })
    test_that("lint::arg, called directly, no default", {
        a <- arg("a number", class="numeric")
        expect_is(a, "argument-info")
        expect_equal(a@info, "a number")
        expect_equal(a@default, .no.default)
        expect_equal(a@classes, "numeric")
    })
    test_that("lint::arg, called directly, no classes", {
        a <- arg("a number", default=1)
        expect_is(a, "argument-info")
        expect_equal(a@info, "a number")
        expect_equal(a@classes, "numeric")
    })
}

`format_Rd.argument-info` <- 
function( x 	#< argument
		, ... 	#< ignored
		){
	#! Format argument for Rd.
	#! @export
	sprintf("\\item{%s}{%s}", x$name, x$description)
}
if(F){#!@ testthat
    x <- arg("x", "an object")
    expect_equal(format_Rd(x), "\\item{x}{an object}")
}
`format_md.argument-info` <- 
function( x		                            #< object
		, indent = 0                        #< indent level
		, bullet = c("*", "+", "-")         #< bullet character
		, ...	#< ignored
		){
	#! format argument for markdown
	#! @export
    bullet=match.arg(bullet)
	paste( paste( rep(" ", getOption("lint::indent.width", 4) * indent), collapse='')
		 , sprintf("%s `%s`, %s", bullet, x$name, x$description)
		 , sep = ''
         )
}
if(F){#!@testthat
    x <- arg("test", 'A test argument class', "any", NULL)
    expect_equal(format_md(x), "* `test`, A test argument class")
}

`print.argument-info` <- 
function( x
		, ...
		, format = c("md", "Rd")
		){
	#! Print a argument-info object.
	format <- match.arg(format)
	formater <- match.fun(paste0("format_", format, ".argument-info"))
	print(formater(x, ...))
}
`as.data.frame.argument-info` <-
function( x
        , ...
        ){
    structure( x
             , row.names = x$name
             , class     = "data.frame"
             )
}	   
	   
.no.default <- structure(expression(), class=c("no-default-arg", "argument-default", "expression"))
find_default <- function(x){
    if(class(x) == "name" && deparse(x)=="")
        return(.no.default)
    y <- as.expression(x)
    structure(y, class = c("argument-default", class(y)))
}
if(F){# Development
    debug(find_default)
    
    f <- function(a, b=3.14, c=a, d = a+b, e=1L){NULL}
    lapply(f, is.language)
    lapply(f, is.numeric)
    lapply(f, typeof)
    lapply(f, mode)
    lapply(f, class)
    lapply(f, deparse)
    lapply(f, find_default)
}


arg_from_string <- 
function( string #< string with information
        , name = deparse(substitute(string)) #< name of the argument.
        , default = .no.default
        ){
    #! Create an `argument-info` object from a string object.
    force(name)
    if(grepl(pat <- "(?<=\\[)[a-zA-Z][^\\]]*(?=\\])", string, perl=TRUE)){
        classes <- 
            gregexpr(pat, string, perl=TRUE) %>%
            regmatches(x=string) %>%
            unlist() %>%
            str_split(fixed("|")) %>%
            unlist() %>% str_trim()
        string <- gsub("^\\[([a-zA-Z][^\\]]*)\\]\\s+", '', string, perl=TRUE)
        string <- gsub("\\[([a-zA-Z][^\\]]*)\\]", '`<\\1>`', string, perl=TRUE)
    } else {
        classes <- "ANY"
    }
    arg(name=name, description=string, classes=classes, default=default)
}
if(F){#!@testthat
    string <- "[class] An object"
    a <- arg_from_string(string)
    expect_is(a, "argument-info")
    expect_is(a, "lint-documentation")
    expect_equal(a$name, "string")
    expect_equal(a$class, "class")
    expect_equal(a$description, "An object")
    
    string <- "[class1|class2] An object"
    a <- arg_from_string(string)
    expect_is(a, "argument-info")
    expect_is(a, "lint-documentation")
    expect_equal(a$name, "string")
    expect_equal(a$class, c("class1", "class2"))
    expect_equal(a$description, "An object")
    
    string <- "The [class] object."
    a <- arg_from_string(string)
    expect_is(a, "argument-info")
    expect_is(a, "lint-documentation")
    expect_equal(a$name, "string")
    expect_equal(a$class, c("class"))
    expect_equal(a$description, "The class object.")
}

{# Arg from Parse Data
pd_arg_name <- 
function(pd){
    #! find argument name from `<parse-data>`.
    pd[1,'text']
}
pd_arg_default <- 
function(pd){
    #< infer default from `<parse-data>`.
    expr.parse <- 
    get_child( id = tail(pd[!is_comment(pd), ], 1)$id
             , pd
             , include.parent = FALSE
             , all.generations = TRUE
             )
    default <- reconstitute_expression(expr.parse)
    if(identical(default, expression())) return(.no.default)
    default
}
pd_arg_comment <- 
function(pd){
    #< get the comment associated with a function argument 
    #^ from`<parse-data>`.
    comment.text <- get_lint_argument_descriptors(pd)$text
    paste(strip_lint_leads(comment.text), collapse=' ')
}
arg_from_pd <- 
function( pd #< The [parse-data] for a single argument.
        ){
    #! Form a `<argument-documentation>` from parse data information.
    arg_from_string( pd_arg_comment(pd)
                   , name    = pd_arg_name(pd)
                   , default = pd_arg_default(pd)
                   )
}
if(F){#!@testthat arg_from_pd
pd <- get_parse_data(arg_from_pd)
pd <- only_arguments(get_pd_assign_value(pd))

a <- arg_from_pd(pd)
expect_is(a, "argument-info")
expect_equal(a$name, "pd")
expect_equal(a$description, "The `<parse-data>` for a single argument.")
expect_equal(a$class, "parse-data")
expect_equal(a$default, .no.default)
}
}


if(F){# functionality discovery
    f <- function(a, b=1, c=a, d=rnorm(1)){}
    valid <- c('call', 'numeric', 'name')
    inherits(formals(f)[['d']], valid)
    inherits(formals(f)[['c']], valid)
    inherits(formals(f)[['b']], valid)

    all.base.names <- ls(asNamespace("base"))
    all.default.classes <- lapply(all.base.names, function(name){
        f <- get(name, envir=asNamespace("base"))
        if(!is.function(f))return(character(0))
        g <- formals(f)
        sapply(g, is.expression)
        sapply(g, class)
    })
    unique.classes <- unique(unlist(all.default.classes))
    a <- sapply(all.default.classes, function(x){"(" %in% x})

    all.base.names[a]


    head(all.default.classes)
}
