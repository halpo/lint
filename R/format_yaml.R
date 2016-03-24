format_md.default <-  
function( x, ...){
	#! @export
	if(!is.null(docs <- attr(x, "docs")))
		return(NextMethod(object=docs))
	stop("no method and no `docs` found for object.")
}

setGeneric("format_yaml", 
    function( x  		#< Object to format
		    , ...		#< Passed on.
		    ){
	#! format an object in YAML format.
	#! @export
	UseMethod("format_yaml")
	#! @return A character string for the object that could be written to a file.
})
if(F){#!@testthat format_md.default
expect_true(isGeneric('format_yaml'))
}

prefix_yaml <- 
function( x #< lines
        , prefix.first = '' #< prefix for the first line
        , prefix.other =    #< prefix for the other lines
            paste(rep(' ', nchar(prefix.first)), collapse='')
        , ...               #< ignored.
        ){
    #! Add prefixes to lines.
    assert_that(is.character(x))
    if(length(x)==0) return(character(0))
    paste0( c( prefix.first, rep(prefix.other, length.out = length(x)-1))
          , x)
}

format_yaml.person <-
function( x   #< Object to format
        , ... #< passed to prefix_yaml
        ){
    assert_that(inherits(x, "person"))
    format( x
          , braces = list( given = c("name: ", ''), family = c("", '\n')
                         , email = c("email: ", "\n")
                         , role = c("role:", "\n")
                         , comment = c("comment:", "\n")
                         )
          , collapse = list(given='', family='', email='', role='', comment='')
          ) %>% 
          gsub("\n ", "\n", .) %>% 
          strsplit("\n", fixed=TRUE) %>%
          lapply(prefix_yaml, ...)
}





