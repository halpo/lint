`format_md.lint-section` <- 
function( x                                 #< A [lint-section] object
        , title  = attr(x, 'section.title') #< [character] Title for the section
        , rewrap = TRUE                     #< [logical] should the lines be rewrapped to `width`?
        , collapse = FALSE                  #< [logical] collapse the section into single string(TRUE) 
                                            #^ or leave as lines(FALSE).
        , ...                               #< passed to `<md_header>`
        , header = md_header(title, ..., collapse=collapse)    #< [character] Starts the section
        , tailer = ''                       #< [character] End the section, s'' represents a blank line.
        , width  = 0.9 * getOption("width") #< [integer]   Width for wrapping.
        ){
    "format a lint-section for markdown."
    force(header)
    if(rewrap) x <- rewrap(x, width)
    x <- c( header, x, tailer)
    if(!collapse) return(x)
    return(paste(x, collapse='\n'))
}
if(F){#! @testthat
    require(stringi)
    section <- 
    structure( stri_rand_lipsum(1)
             , class = c( "lint-section"
                        , "lint-documentation", 'character')
             , section.title = "Lorem"
             )
    out <- format_md(section, rewrap=FALSE)
    expect_true(length(out)==3)
    expect_equal(head(out, 1), "# Lorem")
    expect_equal(tail(out, 1), "")

    out <- format_md(section, title="Ipsum")
    eqpect_true(length(out)>3)
    expect_equal(head(out, 1), "# Ipsum")

    out <- format_md(section, title="A `Lorem Ipsum` section")
    expect_equal(head(out, 1), "# A `<code{Lorem Ipsum>` section")
    
    out <- format_md(section, level=2)
    expect_equal(head(out, 1), "## Lorem")
    
    out <- format_md(section, level=1, style='s')
    expect_equal(head(out, 2), c("Lorem", "=====") )
    
    out <- format_md(section, level=2, style='s')
    expect_equal(head(out, 2), c("Lorem", "-----") )
}

`format_md.lint-description` <-
function(x, ...){NextMethod("format_Rd", x, title="Description")}
`format_md.lint-details` <-
function(x, ...){NextMethod("format_Rd", x, title="Details")}
`format_md.lint-subsection` <-
function(x, ...){NextMethod("format_Rd", x, level=2)}


`format_Rd.lint-section` <-
function( x                                 #< A [lint-section] object
        , title  = attr(x, 'section.title') #< Title for the section
        , rewrap = FALSE                    #< should lines be rewrapped?
        , collapse = FALSE                  #< [logical] collapse the section into single string(TRUE) 
                                            #^ or leave as lines(FALSE).
        , ...                               #< Ignored.
        , header = sprintf("\\section{%s}{", md2rd(title)) #< Starts the section
        , tailer = '}'     #< End the section
        , width  = 80                       #< width to wrap to.
        ){
    #! Format a `lint-section` for Rd format.
    lines <- md2rd(x)
    if(rewrap) lines <- rewrap(lines, width)
    lines <- c( header
              , lines
              , tailer
              )
     if(!collapse) return(lines)
     return(paste(lines, collapse="\n"))
}
if(F){#!@testthat
    require(stringi)
    section <- 
    structure(stri_rand_lipsum(1)
             , class = c( "lint-section"
                        , "lint-documentation", 'character')
             , section.title = "Lorem"
             )
    out <- format_Rd(section)
    expect_true(length(out)==3)
    expect_equal(head(out, 1), "\\section{Lorem}{")
    expect_equal(tail(out, 1), "}")

    out <- format_Rd(section, title="Ipsum")
    expect_equal(head(out, 1), "\\section{ipsum}{")

    out <- format_Rd(section, title="A `Lorem Ipsum` section")
    expect_equal(head(out, 1), "\\section{A \\code{Lorem Ipsum} section}{")
    
    out <- format_Rd(section, rewrap=TRUE)
    expect_true(length(out)>=3, "rewrap")
    
    out <- format_Rd(section, rewrap=TRUE, collapse=TRUE)
    expect_true(length(out)==1, "rewrap & collapse")
    
    out <- format_Rd(section, header = "\\note{")
    expect_equal(head(out, 1), "\\note{")

    out <- format_Rd(section, tailer = "}%end section")
    expect_equal(tail(out, 1), "}%end section")
}

`format_Rd.lint-description` <-
function( x     #< A [lint-description] object
        , ...   #< passed on.
        ){
    NextMethod("format_Rd", x, header="\\description{")
}
if(F){#!@testthat
    require(stringi)
    description <- 
    structure(stri_rand_lipsum(1)
             , class = c("lint-description", "lint-section"
                        , "lint-documentation", 'character')
             )
    out <- format_Rd(description)
    expect_true(length(out)==3)
    expect_equal(head(out, 1), "\\description{")
    expect_equal(tail(out, 1), "}")
    
    out <- format_Rd(description, rewrap=TRUE)
    expect_true(length(out)>=3)
    
    out <- format_Rd(description, rewrap=TRUE, collapse=TRUE)
    expect_true(length(out)==1)
}
`format_Rd.lint-details` <-
function( x     #< A [lint-description] object
        , ...   #< passed on.
        ){
    NextMethod("format_Rd", x, header="\\details{")
}
if(F){#!@testthat
    require(stringi)
    details <- 
    structure(stri_rand_lipsum(1)
             , class = c("lint-details", "lint-section"
                        , "lint-documentation", 'character')
             )
    out <- format_Rd(details)
    expect_true(length(out)==3)
    expect_equal(head(out, 1), "\\details{")
    expect_equal(tail(out, 1), "}")
    
    out <- format_Rd(details, rewrap=TRUE)
    expect_true(length(out)>=3)
    
    out <- format_Rd(details, rewrap=TRUE, collapse=TRUE)
    expect_true(length(out)==1)
}


if(F){#development
    using(stringi)
    x <- stri_rand_lipsum(3)
    x <- llply(x, strwrap, 80)
    x <- unlist(rbind(x, replicate(length(x), '', simplify=F))) %>% head(-1)
    attr(x, 'section.title') <- "Testing"
}


