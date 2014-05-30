{###############################################################################
# selfdocumenting.R
# Andrew Redd
# 2013-10-06
# 
# DESCRIPTION
# ===========
# functions for self documenting functions
# 
}###############################################################################
arg <- function(info="", x, default, class){
    if(!missing(x) && missing(default) && missing(class) && missing(info)){
        if(inherits(x, 'argument-information')){
            return(x)
        }
    }
    default <- 
        if(!missing(default)) as.character(default)[1] else
        if(!missing(x)) deparse(x) else
        ""
    class <-
        if(!missing(class)) class else
        if(!missing(x)) base::class(x)[[1]] else
        'any'
    class <- ifelse(class=='name', 'unknown', class)
    class <- ifelse(class=='call', 'unknown', class)
    structure(
        data.frame(default=default, class, info)
        , class=c('argument-information', 'data.frame'))
}
self_documenting <- function(
      fun        #< Function to document
    , doc.string #< String of Documentation
    , ...        #< Other Provided Information
){
    fun.args <- args(fun)
    classes <- utils::head(lapply(fun.args, arg, info=""), -1)
    arg.information <- 
    Reduce(rbind, mapply(data.frame, name=names(classes), classes, SIMPLIFY=F))

    
    provided.information <- list(...)
    if(length(provided.information)){
        matches <- match(names(provided.information), arg.information$name)
        stopifnot(all(!is.na(matches)))
        
        is.arg.info <- 
        sapply(provided.information, inherits, 'argument-information')
        if(any(is.arg.info)){
            arg.information[matches[is.arg.info], -1] <- 
            Reduce(rbind, provided.information[is.arg.info])
        }
        if(any(!is.arg.info)){
            stopifnot(all(sapply(is.character, X= provided.information[!is.arg.info])))
            arg.information[matches[!is.arg.info], 'info'] <- 
                unlist(provided.information[!is.arg.info])
        }
        
    }
    structure(fun
        , class=c("selfDocumentingFunction", "function")
        , doc.string = doc.string
        , arg.info = arg.information
        )
}
print_args <- function(f){
    body(f) <- substitute()
    environment(f) <- .GlobalEnv
    print(f)
}
print.selfDocumentingFunction <- function(x,...){
    cat(strwrap(attr(x, 'doc.string'), indent=0), '\n\n')
    print_args(x)
    print(attr(x, 'arg.info'))
}

if(F){ #Debugging and Testing
    fun <- rnorm
    provided.information <- 
    list( n=arg('number of random values to return', class='integer')
        , mean='self explanatory'
        )
    
    arg(info='hello world')
    arg(arg(info='hello world'))
    
    norm <- self_documenting(function(x){sqrt(sum(x*x))}
               , "Compute the Norm of a vector"
               , x = arg(class='numeric', 'vector to norm')
               )
               
               
    debug(self_documenting)
    fun <- self_documenting(
    function(x #< an argument
            ){
        print(x)
        return(invisible(x)) #< invisibly returns x.
    })
 
    srcref <- attr(fun, 'srcref')
    srcfile <- attr(srcref, 'srcfile')
    
    ls(srcfile)
    lines = srcfile$lines
    srcfile$filename
    srcfile$fixedNewlines
    
    parse.data <- getParseData(parse(text=lines, keep.source=TRUE))
    
}
