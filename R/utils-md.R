{###############################################################################
# utils-md.R
# (C) Andrew Redd
# 2014-01-21
# 
# This file is part of the R package lint.
#
# lint is free software and it's distribution and use governed by the terms
# of the GNU General Public License version 3 or greater. lint is distributed 
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# http://www.gnu.org/licenses/gpl.html
# 
# DESCRIPTION
# -----------
# Markdown conversion utilities to support markdown syntax in Lint documentation
# 
# 
}###############################################################################
shuffle <- function(x, y){
    #! Shuffle two vectors
    #!
    #! Mix `y` into `x` with the pattern
    #! `c(x[1], y[1], x[2], y[2], ...`
    #! x or y will not be  mixed in.
    stopifnot(class(x) == class(y))
    z <- vector(class(x), length(x)+length(y))
    j <- 1
    for(i in 1:min(length(x),length(y))){
        if(i <= length(x)){
            z[j] <- x[i]
            j <- j + 1
        }
        if(i <= length(y)){
            z[j] <- y[i]
            j <- j +1
        }
    }
    i <- i+1
    if(i < length(x)){
        z[j:length(z)] <- x[i:length(x)]
    } else if(i < length(y)){
        z[j:length(z)] <- y[i:length(y)]
    }   
    return(z)
}
if(FALSE){#! @testthat
    x <- (0:4)*2 +1
    y <- (1:5)*2
    z <- shuffle(x, y)
    expect_equal(z, 1:10)
    
    x <- 1:5
    y <- c(0L,0L)
    expect_equal(shuffle(x, y), c(1,0,2,0,3,4,5))
}

collapse <- function(..., collapse='\n'){
    #! collapse a character vector
    paste(..., collapse=collapse)
}
collapse0 <- function(..., collapse=''){
    paste0(..., collapse=collapse)
}

.nomd <- function(string, escape.char="\\"){
    #! Exclude a region from markdown conversion.
    stopifnot(is.character(string))
    rx <- paste0( "(?<!\\", escape.char, ")@nomd{"
                , "(?<text>.*?)"
                , "(?<!\\", escape.char, ")}@"
                )
    if(any(grepl(rx, string, perl=TRUE))){
        full.match <- gregexpr(rx, string, perl=TRUE) %>% 
                        regmatches(x=string) %>% unlist %>%
                        unique
        ids <- random_id(length(full.match))
        extracted <-  gsub(full.match, pattern=rx, replace="\\1", perl=TRUE) %>%
            structure(names=ids)
        
        for(i in 1:length(ids))
            string <- gsub(full.match[[i]], ids[[i]], string, fixed=TRUE)
        
        structure(string, replacement=extracted, nomd.found=TRUE)
    } else return(string)
}
.un_nomd <- function(string, replacement=getAttr(string, "replacement", character(0))){
    for(i in seq_along(replacement))
        string <- gsub(names(replacement)[[i]], replacement[[i]], string, fixed=TRUE)
    return(noattr(string))
}
.rm_nomd <- function(...){.un_nomd(.nomd(...))}
if(FALSE){#! @testthat .nomd
    expect_equal( .nomd(letters) , letters) 
    
    string <- .T( "links are made with @nomd{<target>}@, "
                , "which translates to \\backslash link\\{target\\}."
                , "here is a @nomd{line}@ with @nomd{multiple}@ @nomd tags."
                )
    result <- .nomd(string)
    expect_true(getAttr(result, "nomd.found"))
    
    expect_equivalent(attr(result, 'replacement'), c("<target>", "line", "multiple"))

    result <- .un_nomd(.nomd("wrapped @nomd{@nomd{}@"))
    expect_equal(result, "wrapped @nomd{")
    
    
    expect_equal(.un_nomd("just a string"), "just a string")
}

md2rd <- 
function( string             #< string with documentation.
        , escape.char = '\\' #< character to escape control sequences.
        , do.nomd = TRUE     #< Should `@nomd{@nomd{}@` tags be excluded?
        )
{#! Convert text from markdown to Rd/LaTeX format.
#! 
#! use @nomd{@nomd{text to exclude}@}@ to exclude from markdown conversion.
#! Applies to code passed through md2rd only not the sub functions.
#! The `@nomd{@nomd{}@` starts and `}@` ends the region.
#! everything in between is not converted to markdown.
#! however the @nomd tag will break any markdown syntax that span the tag.
#! 
#! @seealso <md2rd_code>, <md2rd_link>

    if(!is.null(dim(string)))   dim(string) <- NULL
    if(is.list(string))         sapply(string, md2rd, escape.char=escape.char, do.nomd=do.nomd)
    if(do.nomd)                 string <- .nomd(string, escape.char = escape.char)
    replacement <- getAttr(string, "replacement", character(0))
    return(.un_nomd(md2rd_code(md2rd_link(string)), replacement))
}
if(FALSE){#! @testthat
    debug(md2rd)
    string <- .T( "links are made with @nomd{<target>}@, "
                , "which translates to \\backslash link\\{target\\}."
                , "here is a `@nomd{line}@` with @nomd{multiple}@ @nomd tags."
                )
    result <- md2rd(string)
    expect_equal(result[[1]], "links are made with <target>, ")
    expect_equal(result[[2]], "which translates to \\backslash link\\{target\\}.")
    expect_equal(result[[3]], "here is a \\code{line} with multiple @nomd tags.")
}
if(FALSE){#! @examples
    md2rd("Just plain text, nothing to do")
    md2rd("Text with an automatic link to a <md2rd>")
    md2rd("Text with an automatic link to a `<md2rd>`, wrapped in code the proper form.")
    md2rd("Don't do this <`md2rd`>, while it will convert correctly it is not correct Rdoc format.")
    md2rd("Text with a fully specified [link](md2rd_link) to a function")
    md2rd("Text mixing location and [`code`](md2rd_code).")
    md2rd("Text mixing location and `[code](md2rd_code)`.")
    md2rd("Exclude with @nomd{<@nomd>}@")
}

md2rd_code <- 
function( string             #< string with documentation.
        )
{
    #! convert backticks to code commands.
    #!
    #! `md2rd_code` should not be called directly.
    #! Call md2rd instead to take advantage of the `@nomd`
    #! structure.
    #! @keywords utilities, internal
    #! [[export]]
    gsub("(?<!\\\\)`([^`]+)`", "\\\\code{\\1}", string, perl=T)
}


md2rd_link <- 
function( string             #< string with documentation.
        )
{
    #! convert angle brackets to link commands.
    #! @keywords utilities, internal
    #! 
    #! This function converts markdown style links into Rdoc style links and html href links.
    #! `md2rd_link` should not be called directly.
    #! Call `<md2rd>` instead to take advantage of the `@nomd` structure.
    #! See the details for different forms.
    
    #! @details 
    #! @subsection long form
    #! Links are fully specified with the form \[text\]\(destination\).
    #! For example, @nomd{[rnorm](=stats::rnorm)}@ converts to @nomd{\\link[=stats::rnorm](rnorm)}@
    #! Since the markdown is translated to Rd links the Rd shortcuts can be utilized.
    #! The previous example could be rewritten as `@nomd{[rnorm](stats)}` with the same end effect
    #! in the final documentation, the actual translated link would be 
    #! @nomd{\verb{\link[stats](rnorm)}}@.
    #! 
    #! **Examples:**
    #!     * @nomd{[rnorm](=stats::rnorm)}@ \rightarrow @nomd{\verb{\link[=stats::rnorm](rnorm)}}@
    #!     * @nomd{[rnorm](stats)}@ \rightarrow @nomd{\verb{\link[stats](rnorm)}}@
    #! 
    #! @subsection html links
    #! Links with html destinations with be converted to `href` latex tags.
    #! Denote html links with full html destinations, `@nomd{[text](http://link.url)}@.
    #! Otherwise an in documentation `verb{\link}` is assumed.
    #! 
    #! **Examples:**
    #!    * `The @nomd{[R-Project](http://www.r-project.org/)}@ \rightarrow 
    #!       @nomd{\verb{\href{http://www.r-project.org/}{R-Project}}}@
    schemes <- .T(http, https, ftp, file, ipp, ipps)
    neg.http.rx <- collapse0("(?!", schemes, ":\\/\\/)")
    
    noesc <- "(?<!\\\\)"
    rx.nohttp <- paste0(noesc,"\\[(.*?)", noesc,"\\]\\(", neg.http.rx, "(.*?)", noesc, "\\)")
    string <- gsub(rx.nohttp, "\\\\link[\\2]{\\1}", string, perl=TRUE)
    
    rx.http <- 
        paste0( noesc,"\\[(.*?)", noesc,"\\]\\("
              , paste0("(", schemes, ":\\/\\/", ".*?)")
              , noesc, "\\)"
              ) %>%
        structure(names=schemes)
    for( rx in rx.http)
        string <- gsub(rx, "\\\\href{\\2}{\\1}", string, perl=T)
    
    #! @subsection Implicit forms
    #! [Pandoc](http://pandoc.org) style implicit links can be made wrapping a string
    #! with angle brackets.  The link destination will be the same as the 
    #! link text. There is no whitespace allowed.
    #! HTML links will also be converted to href tags as well.
    #! at this time email addresses are not supported.
    #! 
    #! **Examples:**
    #!      * `@nomd{<target>}@` \rightarrow `\\link\{target\}`
    #!      * `@nomd{<http://target.url>}@ \rightarrow `\\url{http://target.url}`
    #!      * `@nomd{<https://target.url>}@ \rightarrow `\\url{https://target.url}`
    string <- gsub(paste0(noesc,"<", neg.http.rx, noesc,"(.+?)>"), "\\\\link{\\1}", string, perl=T)
        
  
    rx.http <- paste0( noesc,"<"
                     , paste0("(", schemes, ":\\/\\/", ".+?)")
                     , noesc, ">"
                     ) %>%
               structure(names=schemes)
    for(rx in rx.http)
        string <- gsub(rx, "\\\\url{\\1}", string, perl=T)


    #! @subsection Forms not supported.
    #! `md2rd_link` does not support pandoc [reference links](http://pandoc.org/README.html#reference-links)
    #! in any form.
    
    #! @seealso <http://spec.commonmark.org/0.24/#links>
    #! @return formatted string
    string
}
if(FALSE){#! @testthat
    expect_equal(md2rd_link("implicit <target>"), "implicit \\link{target}")
    expect_equal(md2rd_link("<http://target.url>"), "\\url{http://target.url}")
    expect_equal(md2rd_link("<https://target.url>"), "\\url{https://target.url}")
    expect_equal(md2rd_link("<file://target.url>"), "\\url{file://target.url}")
    expect_equal(md2rd_link("[R-Project](http://www.r-project.org/)"), "\\href{http://www.r-project.org/}{R-Project}")
    expect_equal(md2rd_link("[rnorm](=stats::rnorm)"), "\\link[=stats::rnorm]{rnorm}")
    expect_equal(md2rd_link("[rnorm](stats)"), "\\link[stats]{rnorm}")
    expect_equal(md2rd_link("[text and target](target)"), "\\link[target]{text and target}")
    expect_equal(md2rd_link("Html [target](http://r-project.org)"), "Html \\href{http://r-project.org}{target}")
    
    expect_equal(md2rd_link("not an implicit [target]"), "not an implicit [target]")
    expect_equal(md2rd_link("[http://target.url]"), "[http://target.url]")
}


md_header <- 
function( text                          #< [character] Header Text
        , level = 1                     #< [integer]   Level of the header.
        , style = c('atx', 'setext')    #< [character] Markdown header style
        , collapse = FALSE              #< should the lines (style=="setext") be collapse.
        , ...                           #< discarded
        ){
    #! Format text as a markdown header according to the style.
    style <- match.arg(style)
    n <- nchar(text)
    if (style=='setext') {
        if (level > 2) {
            warning("Setext style only applies to level<=2, using atx style.")
        } else {
            lines <- c( text, paste(rep(c('=', '-')[level], max(nchar(text), 5)), collapse='') )
            if(collapse) return(paste(lines, collapse='\n'))
            else return(lines)
        }
    }
    rep("#", level) %>% c(' %s') %>% paste(collapse='') %>% sprintf(text) %>% return
}
if(FALSE){#! @testthat
    text <- "Section Title"
    expect_equal(md_header(text), "# Section Title")
    expect_equal(md_header(text, level=4), "#### Section Title")
    expect_equal(md_header(text, level=1, style='se'), c("Section Title", "============="))
    expect_equal(md_header(text, level=2, style='se'), c("Section Title", "-------------"))

    expect_warning(md_header(text, level=3, style='se'))
}



