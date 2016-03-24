{#######################################################################
# self_documenting.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Function documentation objects.
#
}#######################################################################

self_documenting <- 
function( fun                               #< function to docment
        , doc.string=find_docstring(fun)    #< title document string
        , ...                               #< arguments for `<functionDocumentation>`
        , pd =getParseData(srcref)  #< Parse data see `<getParseData>`
        ){
    #! create a function with documentation attached.
    #! 
    #! Provides and alternative to the document_function construct in that it 
    #! assumes that the sections are null and the `...` identifies the arguments.
    #! self_documenting assumes that all documentation is contained inside the 
    #! function body.
    
    fun <- document_function(fun, pd=pd)
    provided <- list(...)
    if(length(provided)==0) return(fun)
    docs = attr(fun, 'docs')
    if(!all(. <- names(provided) %in% names(docs$arguments)))
        stop("The following provided are not arguments names: ", names(provided)[!.])
    
    for(i in seq(length(provided))){
        p    <- provided[[i]]
        name <- names(provided)[[i]]
        if(inherits(p, 'arg')) {
            if(is.null(p$name)) p$name <- name
            else if(p$name != name) stop("Named argument", sQuote(name), "does not match argument name.")
            if(!(name %in% names(docs$arguments))) stop(sQuote(name), "was provided but is not an argument name")
            docs$arguments[[name]] <- p
        } else if(inherits(p, 'character')){
            docs$arguments[[name]] <- arg_from_string(string=p, name=name)
        } else {
            stop("argument", sQuote(name), "at position", i, "is not valid.")
        }
    }

    structure( fun
             , class = c("selfDocumentingFunction", "function")
             , docs  = docs
             )
}
