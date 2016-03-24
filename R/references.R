
extract_references <- function(pd){
    #! Extract references from from parse data 
    #! 
    #! References are specified in [yaml] with fenced code blocks
    #! 
    #! 
    tagged.lines <- extract_tagged_lines(pd, "references")
    if(nrow(tagged.lines)==0) return(NULL)
    rx <- "^([~`])(?: |\\1)+@references"
    ix <- grepl(rx, strip_lint_leads(tagged.lines$text), ignore.case=TRUE, perl=TRUE)
    if(sum(ix)==0) return(NULL)
    if(sum(ix)>=2) stop("multiple opening @reference sections found")
    start.line <- tagged.lines[ix,]
    
    fence <- gsub(" ", '', strip_tag(strip_lint_leads(start.line$text), "references"))
        
    #! as per the CommonMark spec
    #! the closing code fence must use the same character as the opening fence,
    #! and must be at least as long as the opening.
    sub.pd <- pd[pd$line1 > start.line$line1, ]
    
    end.ix   <- grepl(paste0("^", fence, "+"), strip_lint_leads(sub.pd$text))
    end.i    <- if(sum(end.ix)>=1) min(which(end.ix)) else length(end.ix)
    end.line <- sub.pd[end.i,]    
    
    ref.pd <- pd[pd$line1 > start.line$line1 & pd$line2 < end.line$line2, ]
    if(!all(ix <- is_lint_comment(ref.pd$text))) {
        warning("Not all lines in the references block are comments.")
        ref.pd <- ref.pd[ix,]
    }
    yaml.references <- yaml.load(collapse( strip_lint_leads(ref.pd$text, space=FALSE)))
    lint.references <- 
        if(is.null(names(yaml.references))){
            Reduce(c,lapply(yaml.references, do.call, what='bibentry'))
        } else {
            do.call(bibentry, yaml.references)
        }
    structure( lint.references
             , class=.T("lint-references", "lint-documentation", "bibentry") 
             , used.lines = ref.pd
             )
}
format_Rd.bibentry<- 
`format_md.lint-references` <- 
function( x     #< object
        , ...   #< discarded? 
        ){
    c( md_header("References")
     , paste("* ", format(x, "citation"))
     )
}
format_Rd.bibentry <-
`format_Rd.lint-references` <- 
function(x, ...){
    c( "\\references{"
     , format(x, "latex")
     , "}"
     )
}
if(FALSE){#! @testthat
test.fun <- function(){
#!  This does noting
#!  
#!  ~~~@references
#!  - bibtype: Manual
#!    title: Commonmark Specification
#!    year: 2016
#!    url: http://spec.commonmark.org/
#!  ~~~
0
}
pd <- get_parse_data(test.fun)
refs <- extract_references(pd)
expect_is(refs, 'bibentry')
expect_equal(refs[[1]]$title, 'Commonmark Specification')
}
if(FALSE){# development
    rm(list=ls())
    load_all()
    fun <- document_function
    pd <- get_pd_assign_value(get_parse_data(fun))
}







