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

#' @export
setClass( "argument-documentation"
        , representation( info    = "character"
                        , classes = "character"
                        , default = "ANY"
                        )
        )

.no.default <- structure(expression(), class=c("no-default-arg", "argument-default", "expression"))
find_default <- function(x){
    if(class(x) == "name" && deparse(x)=="")
        return(.no.default)
    y <- as.expression(x)
    structure(y, class = c("argument-default", class(y)))
}
if(F){
    debug(find_default)
    lapply(function(a, b=1, c=a, d = a+b){NULL}, as.expression)
    lapply(function(a, b=1, c=a, d = a+b){NULL}, class)
    lapply(function(a, b=1, c=a, d = a+b){NULL}, find_default)
}
        
#' @export
arg <- function( info=""    #< Information String.
               , x          #< Argument, an expression from an apply over a function 
               , default    #< Default. 
               , classes    #< Allowed Classes
               ){
    #' Create an argument object
    
    
    #TODO{Andrew}: Program extracting info strings from comments.
    if(!missing(x) && missing(default) && missing(class) && missing(info)){
        if(inherits(x, 'argument-documentation')){
            return(x)
        }
    }
    default <- 
        if(!missing(default)) default else
        if(!missing(x)) find_default(x) else 
        .no.default
    classes <-  # TODO program extracting from documentation strings
        if(!missing(classes)){ 
            if(length(classes)==1 && tolower(classes) == "any")
                structure("ANY", any.class=TRUE)
            classes 
        } else if(!missing(x) ){
            if(inherits(default, "no-default-arg") 
               || base::class(x)[[1]] == "name")
                structure("UNKNOWN", unknown.classes=TRUE)
            else
                base::class(x)[[1]]
        } else {            
            structure("ANY", any.class=TRUE)
        }
    classes <- ifelse(classes=='name', 'unknown', classes)
    classes <- ifelse(classes=='call', 'unknown', classes)
    new("argument-documentation", default=default, classes=classes, info=info)
}
if(F){# @TESTTHAT
    test_that("lint::arg, called directly.", {
        a <- arg("a number", class="numeric", default=1)
        expect_is(a, "argument-documentation")
        expect_equal(a@info, "a number")
        expect_equal(a@default, 1)
        expect_equal(a@classes, "numeric")
    })
    test_that("lint::arg, called directly, no default", {
        a <- arg("a number", class="numeric")
        expect_is(a, "argument-documentation")
        expect_equal(a@info, "a number")
        expect_equal(a@default, .no.default)
        expect_equal(a@classes, "numeric")
    })
    test_that("lint::arg, called directly, no classes", {
        a <- arg("a number", default=1)
        expect_is(a, "argument-documentation")
        expect_equal(a@info, "a number")
        expect_equal(a@classes, "numeric")
    })
    
    
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
