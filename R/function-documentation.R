########################################################################
# functionDocumentation.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Function documentation objects.
#
########################################################################
#' @inport class-lintDocumentation.R


function_documentation <- 
function( name          = NULL #< [name/character] Name of the function
        , title         = NULL #< [character] Title
        , description   = NULL #< [character] Detailed Desctiption
        , usage         = NULL #< [call] the usage string
		, arguments		= NULL #< [argList] arguments of the function.
		, details		= NULL #< [character] details section.
        , return        = NULL #< [character] Return Value
		, references	= NULL #< List of references.
        , author		= NULL #< [person|list] The `<author>` as a person object.
		, seealso       = NULL #< [character] See also
        , examples      = NULL #< [character/list] example code    
        , keywords      = NULL #< [character] Keywords
        , date          = NULL #< [POSIXlt] Date of authorship.
		, ...                  #< extra information
		){
    "create an object of class `function-documentation`."
    if(!is.null(description) && !inherits(description, 'lint-description'))
        description <-     
            structure(description, class=c("lint-description", "lint-section", "lint-documentation", class(description)) )
    if(!is.null(details) && !inherits(details, 'lint-details'))
        details <-     
            structure(details,  class=c("lint-details", "lint-section", "lint-documentation", class(details)) )
    
    structure(
        list( name          = name          
            , title         = title         
            , description   = description
            , usage         = usage         
            , arguments		= arguments		
            , details		= details		
            , return        = return         
            , references	= references	
            , author		= author		
            , seealso       = seealso       
            , examples      = examples      
            , keywords      = keywords      
            , ...
            )
        , class = c("function-documentation", "lint-documentation", "list"))
}

`print.function-documentation` <-
function( x             #< A [function-documentation] object.
        , usage = TRUE  #< [logical] Should the usage be printed?
        , ...
        ){
    #! print a `function-documentation` object.
    format_md(x, include=.T('title', usage, arguments, description, return), metadata='titleonly') %>%
    cat(sep='\n')

    invisible(x)
    #! [Export]
}


#' @importFrom yaml as.yaml
#' @importFrom magrittr `%<>%` `%>%` `%$%`
`format_md.function-documentation` <-
function( x   #< A [function-documentation] object
        , ... #< Passed along
        , include='all'  #< what to include, either all or names or elements to include.
        , metadata = c('yaml', 'pandoc', 'titleonly', 'none') 
        , collapse = FALSE #< collapse lines?
        ){
    #! format function documentation for markdown.
    opar <- options(useFancyQuotes = FALSE)
    on.exit(options(opar))
    if(length(include) && !identical(include, ''))
        include = match.arg(include, c('all', names(x)), several.ok=TRUE)
    if('all' %in% include){
        if(length(include) > 1) warning("for `include`, 'all' should not be specified with any other name.")
        include  = names(x)
    } 


    #! There is a YAML meta data block that includes the title author and date information.
    metadata <- match.arg(metadata)
    metadata.block <- switch(metadata
        , yaml    = { x[intersect(.T(name, title, author), names(x))] %>%
                      as.yaml %>%
                      strsplit("\n") %>% unlist %>%
                      str_trim %>%
                      c( if(!is.null(x$date)){ format(x$date) }
                       , if(!is.null(x$keywords)){ paste0("keywords: [", paste(x$keywords, collapse=', '), "]") }
                       ) %>%
                      c('---', ., "...", '')
                    }
        , pandoc  = { c(x$title, format(x$author), format(x$date)) %>%
                      paste('%', .) %>% c('') 
                    }
        , titleonly = {c(x$title, '')}
        , character(0))

    formatted.docs <- metadata.block
    if(!is.null(x$description) && 'description' %in% include) formatted.docs <- c(formatted.docs, "# Description", x$description, '')
    if(!is.null(x$usage      ) && 'usage'       %in% include) formatted.docs <- c(formatted.docs, "# Usage"      , format(x$usage), '')
    if(!is.null(x$arguments  ) && 'arguments'   %in% include) formatted.docs <- c(formatted.docs, format_md(x$arguments, header = "# Arguments", collapse=collapse), '')
    if(!is.null(x$details    ) && 'details'     %in% include) formatted.docs <- c(formatted.docs, "# Details"    , x$details, '')
    if(!is.null(x$return     ) && 'return'      %in% include) formatted.docs <- c(formatted.docs, "# Value"      , x$return , '')
    if(!is.null(x$seealso    ) && 'seealso'     %in% include) formatted.docs <- c(formatted.docs, "# See Also"   , x$seealso, '')
    if(!is.null(x$sections   ) && 'sections'    %in% include) formatted.docs <- c(formatted.docs, unlist(sapply(x$sections, format_md)), '')
    if(!is.null(x$references ) && 'references'  %in% include) formatted.docs <- c(formatted.docs, "# References" , format_md(x$references), '')
    if(!is.null(x$examples   ) && 'examples'    %in% include) formatted.docs <- c(formatted.docs, "# Examples"   , x$examples, '')

    return(.rm_nomd(formatted.docs))
    #! @return a character vector with lines of the formatted docs.
}
if(FALSE){#!@testthat
    x <- getAttr(document_function(document_function), 'docs', NULL)
    # metadata block
    ## yaml
    y <- format_md(x, include=character(0))
    expect_equal(head(y,1), "---")
    expect_equal(tail(y,2)[1], "...")
    expect_true("title: Document a Function from Source Code" %in% y)
    ## pandoc
    y <- format_md(x, include=character(0), metadata = "pandoc")
    expect_equal(y, c( "% Document a Function from Source Code"
                     , "% Andrew Redd <Andrew.Redd@hsc.utah.edu>"
                     , "% 2016-01-13", ""))
    
    ## titleonly
    y <- format_md(x, include=character(0), metadata="titleonly")
    expect_equal(y, c("Document a Function from Source Code", ""))
    
    # description
    y <- format_md(x, include=.T(description), metadata="none")
    expect_equal(head(y,1), "# Description")
    expect_equal(tail(y,1), "")
    
    # usage
    y <- format_md(x, include=.T(usage), metadata="none")
    expect_equal(head(y,1), "# Usage")
    expect_equal(tail(y,1), "")
    
    # arguments
    y <- format_md(x, include=.T(arguments), metadata="none")
    expect_equal(head(y,1), "# Arguments")
    expect_equal(   y[[2]], "    * `fun`, a `<function>` object")
    expect_equal(tail(y,1), "")
    
    # details
    y <- format_md(x, include=.T(details), metadata="none")
    expect_equal(head(y,1), "# Details")
    expect_equal(tail(y,1), "")
    
    # return
    y <- format_md(x, include=.T(return), metadata="none")
    expect_equal(head(y,1), "# Value")
    expect_equal(tail(y,1), "")
    
    # seealso
    y <- format_md(x, include=.T(seealso), metadata="none")
    expect_equal(head(y,1), "# See Also")
    expect_equal(tail(y,1), "")
    
    # sections
    # references
    # examples
}

`format_Rd.function-documentation` <-
function( x             #< A [function-documentation] object
        , ...           #< passed along
        , include='all' #< what to include, either all or names or elements to include.
        ){
    include = match.arg(include, c('all', names(x)), several.ok=TRUE)
    if('all' %in% include){
        if(length(include) > 1) warning("for `include`, 'all' should not be specified with any other name.")
        include  = names(x)
    } 
    unlist(
    c( if(!is.null(x$name       ) && 'name'        %in% include) sprintf("\\name{%s}" , x$name)
     , if(!is.null(x$alias      ) && 'alias'       %in% include) sprintf("\\alias{%s}", x$alias)
     , if(!is.null(x$title      ) && 'title'       %in% include) sprintf("\\title{%s}", md2rd(x$title))
     , if(!is.null(x$description) && 'description' %in% include) c("\\description{", rewrap(md2rd(x$description), 80), "}" )
     , if(!is.null(x$usage      ) && 'usage'       %in% include) sprintf("\\usage{%s}", format(x$usage))
     , if(!is.null(x$arguments  ) && 'arguments'   %in% include) format_Rd(x$arguments, ...)
     , if(!is.null(x$details    ) && 'details'     %in% include) format_Rd(x$details, ...)
     , if(!is.null(x$return     ) && 'return'      %in% include) sprintf("\\value{%s}", paste(md2rd(x$return), collapse=' '))
     , if(!is.null(x$seealso    ) && 'seealso'     %in% include) sprintf("\\seealso{%s}", paste(md2rd(x$seealso), collapse=' '))
     , if(!is.null(x$sections   ) && 'sections'    %in% include) sapply(x$sections, format_Rd, ...)
     , if(!is.null(x$references ) && 'references'  %in% include) format_Rd(x$references, ...)
     , if(!is.null(x$examples   ) && 'examples'    %in% include) c("\\examples{", x$examples, "}")
     ))
}
if(FALSE){#! @testthat
    stopifnot(require(stringi))

    docs <- function_documentation( name        = "test"
                                  , alias       = c("alias1", "alias2")
                                  , title       = "The Documenation Title"
                                  , description = "a short description"
                                  , usage       = call('test', as.name('...'))
                                  , arguments   = arguments_list('...' = arg('...', 'arbitrary arguments'))
                                  , details     = stri_rand_lipsum(3)
                                  , return      = "used for testing only"
                                  , seealso     = "`<document_function>`, `<format_Rd>`"
                                  , sections    = list( structure( stri_rand_lipsum(3), section.title='More Info'
                                                                 , class = c('lint-section', 'lint-documentation', 'character') )
                                                      )
                                  , references  = bibentry("Manual", title="Markdown Specification", year=2016, url="http://spec.commonmark.org/")
                                  , examples    = c("test(1,2,3)")
                                  )
    formatted <- format_Rd(docs)
    expect_true("\\name{test}" %in% formatted)
    expect_true("\\alias{alias1}" %in% formatted)
    expect_true("\\alias{alias2}" %in% formatted)
    expect_true("\\title{The Documenation Title}" %in% formatted)
    expect_true("\\description{" %in% formatted)
    expect_true("\\usage{test(...)}" %in% formatted)
    expect_true("\\arguments{" %in% formatted)
    expect_true("\\item{...}{arbitrary arguments}" %in% formatted)
    expect_true("\\details{" %in% formatted)
    expect_true("\\value{used for testing only}" %in% formatted)
    expect_true("\\value{used for testing only}" %in% formatted)
    expect_true("\\seealso{\\code{\\link{document_function}}, \\code{\\link{format_Rd}}}" %in% formatted)
    expect_true("\\section{More Info}{" %in% formatted)
    expect_true("\\examples{" %in% formatted)
    expect_true("\\references{" %in% formatted)
}
