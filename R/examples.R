
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