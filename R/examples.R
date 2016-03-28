
find_fenced <-
function( pd        #< parse data.
        , tag=NULL  #< tag to filter for.  If NULL return all fenced blocks.
        , allow.breaks = NA #< should breaks in fenced sections be allowed.
                            #^ NA(default) means warning,
                            #^ TRUE mean allow without warning,
                            #^ FALSE mean throw error if breaks are found.
        ){
    #! find a fenced region in comments.
    #! fenced regions must be on consecutive lines
    #! however they may be be preceeded by R commands.

    pd   <- get_lint_comments(pd)

    blocks <- list()
    if(nrow(pd)==0) return(blocks)

    text <- strip_lint_leads(pd$text)
    fence.rx <- "^([~`])(?: *\\1){2,}([^~`]*)$"
    while(TRUE){
        found <- grepl(fence.rx, text, ignore.case=TRUE, perl=TRUE)
        if(!any(ix <- found>0)) break

        open <- min(which(ix))
        if(open >= length(text)) break

        open.ticks <- text[open] %>% gsub("([`~]{3,}).*$", "\\1", .) %>% gsub(" ", "", .)
        stopifnot(grepl("^([`~])\\1{2,}", open.ticks, perl=TRUE, ignore.case=TRUE))
        info.string <- gsub(fence.rx, "\\2", text[open], perl=TRUE, ignore.case=TRUE)

        close.rx <- sprintf("%s{%d,}", substring(open.ticks, 1L, 1L), nchar(open.ticks))

        ic <- grepl(close.rx, tail(text, -open), perl=TRUE, ignore.case=TRUE)
        if(!any(ic)) break
        close <- min(which(ic))+open

        if(!all(is_consecutive(pd[open:close,'line1']))){
            if(is.na(allow.breaks)){
                warning("Broken fence block found.")
            } else if(!allow.breaks){
                error("Broken fence block found.")
            }
        }
        block <- structure( text[(open+1):(close-1)]
                          , info.string = info.string
                          , used.lines  = pd[open:close,]
                          )
        if(!is.null(tag)){
            if(grepl(paste0("@", tag), info.string, perl=TRUE, ignore.case=TRUE)){
                blocks <- append(blocks, list(block))
            }
        } else {
            blocks <- append(blocks, list(block))
        }
        text <- text[-(1:close)]
        pd   <- pd[-(1:close),]
    }
    return(blocks)
    #! @return a list of the fenced sections.
    #^  If tag is provided then the list is reduced to those matching the tag in the info string.
}
if(FALSE){#! @testthat
fence.rx <- "^([~`])(?: *\\1){2,}([^~`]*)$"
cases <- c( "``` yes"
          , "`  no"
          , "` `  ` yes"
          , "` `  ` yes"
          , "` `  no"
          , "~~~Yes"
          , "~`~`~No"
          , "``` x~y No"
          , "~~~ x~y No"
          )
expect_equal( grepl(fence.rx, cases, perl=TRUE)
            , c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
            )

"hw <- function(){
    #! ```@examples
    #! hw()
    #! ```
    print('Hello World')
}
" %>%
parse(text=.) %>%
get_parse_data() -> pd

expect_equivalent( find_fenced(pd), list("hw()"))

# Two fenced sections
"hw <- function(){
    #! ```@example 1
    #! hw()
    #! ```
    #! ~~~ code section not tagged
    #! cat('Hello World\\n')
    #! ~~~
    print('Hello World')
}
" %>%
parse(text=.) %>%
get_parse_data() -> pd

result <- find_fenced(pd)
expect_equal(length(result), 2)
expect_equal(attr(result[[1]], "info.string"), "@example 1")
expect_equal(attr(result[[2]], "info.string"), " code section not tagged")

example <- find_fenced(pd, "examples?")
expect_equal(length(example), 1)
expect_equivalent(example[[1]], "hw()")


# ill-formed fences.
"hw <- function(){
    #! ~~~@example 1
    #! hw()
    #! ``` code section not tagged
    #! cat('Hello World\\n')
    #! ```
    print('Hello World')
}
" %>%
parse(text=.) %>%
get_parse_data() -> pd

expect_equal(length(find_fenced(pd)), 0)


}

extract_examples <- function(pd){
    #! Extract code examples from parse data.
    #!
    #! This function extracts example tagged [code blocks](http://spec.commonmark.org/0.24/#fenced-code-blocks)
    #! that are embedded into the comments of a function.
    #! inline examples are specified with code blocks that start with
    #! three or more back ticks @nomd{"`"}@ or tildes "@nomd{~}@", called the opening fence,
    #! followed by the tag @nomd{@examples}@ to denote an example block.
    #! The block is ended by the closing fence, which must use the same character
    #! as the opening fence and have at least as many.
    #!
    #! The preferred method for specifying code examples is
    #! with a `@nomd{if(FALSE){#! @examples}}@` code block.

    tag <- "examples?"
    blocks <- find_fenced(pd, tag)
    if(!length(blocks)) return(NULL)
    examples <- character(0)
    used.lines <- NULL
    for(block in blocks){
        is <- str_trim(strip_tag(attr(block, "info.string"), tag))
        if(nchar(is)) examples <- append(examples, paste("#", is))
        examples <- append(examples, block)
        used.lines <- rbind(used.lines, attr(block, 'used.lines'))
    }
    return(structure(examples, used.lines=used.lines))
}
if(FALSE){#! @testthat
"hw <- function(){
    #! ```@examples
    #! hw()
    #! ```
    print('Hello World')
}
" %>%
parse(text=.) %>%
get_parse_data() -> pd
ex <- extract_examples(pd)

expect_equivalent(ex, "hw()")
expect_equal(attr(ex, "used.lines")$line1, 2:4)

}


#TODO: Extract examples from `if(F){#!@examples blocks in files}

extract_block_examples <-
function( pd        = NULL     #< Parse data for file, will parse `file.in` if missing.
        , pkg.env   = topenv(parent.frame()) #< Environment containing the objects
        ){
    #! Extract blocks tagged as examples and associate with objects
    tag <- "examples?"
    if( is.null(pd) )stop("Either pd must be provided to extract_block_examples")
    pd <- sort(pd)
    #! Examples can be placed inside the same files as the source
    #! for the functions.  Wrap the lines in curly braces and place
    #! `if(FALSE)` before the opening brace, to denote that the code
    #! should not be run when sourced, such as when building a package.
    #! The `FALSE` may be abbreviated as `F`, but those are the only
    #! two acceptable options.  Also required is a lint comment with a
    #! tag denoting that the block is for testing,
    #! either `@nomd{@example}@` or `@nomd{@examples}@`.
    #! The comment must be a lint style comment either `#!` or `#<`,
    #! regular comments are ignored.  The `if` the opening brace `{`
    #! and the tag must all be on the same line.
    comments <- extract_tagged_lines(get_lint_comments(pd), tag)
    families <- lapply(comments$id, get_family, pd=pd, nancestors=2)
    blocks   <- filter_if_Fblock_tagged(families, tag)
    for(block in blocks){
        if(getAttr(block, "string.info", '')==''){
            #! After the tag you may provide an information
            #! string.  At the moment the information string is
            #! only used for naming the object the example should be
            #! associated with.
            #!
            #! If the user does not provide the information string
            #! it will be infered as the name of the assignment which
            #! immediately preceeded the block(s).  If the
            #! previous expression was not an assignment
            #! or a similar [testing block](extrac_block_testthat)
            #! the info string, must be provided.
            #!
            id <- attr(block, 'block.root')
            prev.ids <- sort(subset(get_family(id, pd, 1, 0), id < test.id)$id, TRUE)
            prev.id <- head(prev.ids, 1)
            while(is_Fblock(prev.pd <- get_family(prev.id, pd)) && sum(prev.ids < prev.id)){
                prev.id <- max(prev.ids[prev.ids < prev.id])
            }
            if(is_pd_assignment(prev.pd)){
                attr(block, 'info.string') <-
                    getParseText(prev.pd, get_pd_assign_variable_id(prev.pd))
            } else stop("Malplaced testing block, see the documentation for `extract_block_examples`.")
        }

        body.id <- attr(block, 'body.id')
        example.ids <- subset( get_child( body.id, block, nlevels=1, include.parent=FALSE)
                             , !(token %in% c("'{'", "'}'"))
                             )$id
        example.text <- getParseText(block, example.ids)

        object.name  <- attr(block, 'info.string')
        if(!exists(object.name, envir=pkg.env)){
            warning("Could not find an object for name ", object.name)
            next
        }
        docs <- attr(get(object.name, pkg.env), 'docs')
        if(is.null(docs)){
            warning("Object ", object.name, " is not a documented object.")
            next
        }
        if(!is.null(docs$examples)){
            example.text <- c(docs$examples, example.text)
        }
        attr(pkg.env[[object.name]], 'docs')$examples <- example.text
    }
}
if(F){#!@examples
text <-"
hello_world <- function(){
    #! The classic Hello World
    print('hello world!')
}
if(FALSE){#! @examples
hello_world
}
"

source(textConnection(text))
hello_world
hello_world <- document_function(hello_world)
pd <- get_parse_data(parse(text=text))
extract_block_examples(pd)
cat(attr(hello_world, 'docs')$examples, "\n")


}
if(F){#!@testthat
'hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}
if(FALSE){#!@examples
    hello_world()
}

f2 <- function(){stop("this does nothing")}
if(F){#! @test
    expect_error(f2())
}
if(F){#! @example
    if(F) f2()
}
' -> text

source(textConnection(text))
pd <- get_parse_data(parse(text=text))
document_package(environment())
extract_block_examples(pd, environment())

expect_equal( attr(hello_world, 'docs')$examples
            , c( "#!@examples", "hello_world()" )
            )

expect_equal( attr(f2, 'docs')$examples
            , c("#! @example", "if(F) f2()")
            )
}




