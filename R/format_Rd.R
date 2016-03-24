format_Rd <- 
function( x  		#< Object to format
		, ...		#< Passed on.
		){
	#! format an object in Rd format.
	#! @export
	UseMethod("format_Rd")
	#! @return A character string for the object that could be written to a file.
}
format_Rd.default <- 
function( x, ...){
	#! @export
	if(!is.null(docs <- attr(x, "docs")))
		return(NextMethod(object=docs))
	stop("no method and no `docs` foudn for object.")
}
