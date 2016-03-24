format_md.default <- 
function( x, ...){
	#! @export
	if(!is.null(docs <- attr(x, "docs")))
		return(NextMethod(object=docs))
	stop("no method and no `docs` found for object.")
}

setGeneric("format_md", function( x  		#< Object to format
		, ...		#< Passed on.
		){
	#! format an object in Rd format.
	#! @export
    UseMethod("format_md")
	#! @return A character string for the object that could be written to a file.
})
if(F){#!@testthat format_md.default
expect_true(isGeneric('format_md'))
}

format_md.NULL <- function(x,...){NULL}