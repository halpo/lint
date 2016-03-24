########################################################################
# document_function.R
# Andrew Redd
# (c) 2015-02-17
#
# DESCRIPTION
# ===========
# Function documentation functions.
#
########################################################################
#' @import function-documentation.R
extract_tagged_lines <- 
function( pd   #< parse data.    
        , tag  #< tag without the @.
        ){
    pattern <- sprintf("(?<!@)(?<!@nomd{)@%s\\b\\s*", tag)
    comments <- subset(pd, token %in% c("LINT_COMMENT", "ROXYGEN_COMMENT"))
    has <- grepl(pattern, comments$text, perl=TRUE, ignore.case=TRUE)
    comments[has,] %>% 
        structure(pattern=pattern, tag=tag)
}
if(FALSE){#! @testthat
    fun <- function(){
        #! function with only lines
        #! @tag should extract
        #! @nomd{@tag}@ should not extract
        #! @nomd{@tag}@ should not extract.
        NULL
    }
    pd <- get_parse_data(fun)
    e <- extract_tagged_lines(pd, 'tag')
    expect_equal(nrow(e), 1)
    expect_true("#! @tag should extract" %in% e$text)
    expect_false("#! @nomd{@tag}@ should not extract" %in% e$text)
    expect_false("#! @nomd{@tag}@ should not extract." %in% e$text)
}

strip_tag <-
function( text  #< text to strip from
        , tag   #< tag to remove
        ){
    #! remove a tag that identified a line.
    pattern <- sprintf("(?<!@nomd{)@%s\\b\\s*", tag)   
    gsub( pattern=pattern, replacement='', text
        , perl=TRUE, ignore.case=TRUE)
    #< text with the @ tag removed.
}
 if(FALSE){#! @testthat
    text <- c( "@tag should be removed"
             , "@nomd{@tag}@ should not be removed"
             , "@nomd{@tag}@ should not be removed."
             )
    e <- strip_tag(text, 'tag')
    expect_true("should be removed" %in% e)
    expect_true("@nomd{@tag}@ should not be removed" %in% e)
    expect_true("@nomd{@tag}@ should not be removed." %in% e)
}

extract_tagged <- 
function( pd  #< parse data
        , tag #< tag without the @
        ){
    #' extract cleaned text for a tag from parse data.
    used <- extract_tagged_lines(pd, tag)
    if(nrow(used)>1){
        stop( "multiple @title tags found on lines:"
            , collapse(title.line, collapse=', ')
            )
    } else if(nrow(used)==1){
        used[,'text'] %>% 
            strip_lint_leads() %>%
            strip_tag(tag) %>%
            structure( used.lines = used
                     , tag        = tag
                     , class      = c(sprintf('lint-%s', tag), 'lint-documentation')
                     )
    } else {
        return(NULL)
    }
    #! @seealso `<string_tag>`, `<extract_tagged_lines>`, `<strip_lint_leads>`
    #! @return a character vector with lint comment markers and any  
    #^         @nomd{@tag}@ tags removed, or a `NULL` value if not found.
}

get_fun_title <-
function( f #< function
        , pd = get_parse_data(f)  #< parse data for `f`.
        ){
    #! @title find the title for a function
    #! 
    #! Looks in the following locations in order
    #! #. first line of the body as a character.
    x <- body(f)[[2]]
    if(is.character(x)){
        return(structure(x, class=c('lint-title', 'lint-documentation')))
    }
    #! #. A `@nomd{@title}@` tag in lint or roxygen comments.
    comments <- subset(pd, token %in% c("LINT_COMMENT", "ROXYGEN_COMMENT"))
    if(is_pd_assignment(pd)) pd <- get_pd_assign_value(pd)
    title <- extract_tagged(pd, 'title')
    if(inherits(title, 'lint-documentation')) return(title)
    #' #. First line of comment in body + any continuation comments.
    stopifnot(is_pd_function(pd))
    pd.body <- pd_function_body(pd = pd)[[1]]
    first.line <- pd.body[3,]
    if(is_lint_comment(first.line$text)){
        comments <- get_associated_continuation(tail(pd.body, -2))
        title <- paste(strip_lint_leads(comments$text), collapse=' ')
        return(
            structure( title
                     , used.lines = comments
                     )
        )
    }
    #' First line of the roxygen style comment.
    # TODO: pull from roxygen comments.
    return(NULL)
}
if(F){#! @testthat
expect_equal(get_fun_title(function_documentation), "create an object of class `function-documentation`.")
expect_equal(get_fun_title(get_fun_title), "find the title for a function")
#TODO Test roxygen titles.


# from Roxygen
"#' Test Function
 #' 
 #' Prints hello world.
 test <- function(){print('hello world')}
" %>% parse(text=.) ->expr
env <- new.env()
eval(expr, envir=env)

expr %>% get_parse_data() ->pd
get_fun_title(env$test, pd=pd)


}

make_usage <- function(fun, name=substitute(fun)){
    #< turn a function into a call
    call <- as.call(append(as.symbol(name), head(as.list(args(fun)), -1)))
    lens <- sapply(sapply(call, deparse), nchar)
    call[lens==0] <- unname(sapply(names(call)[lens==0], as.name))
    names(call)[lens==0] <- ""
    call
    #! @return a call formed from the function.
}
#TODO: Extend `make_usage` to other objects besides functions, such as data.frames.

extract_seealso <- 
function( pd                                #< parse data.
        ){
    #! extract @nomd{@seealso}@ tag values.
    used <- extract_tagged_lines(pd, "seealso")
    if(nrow(used)==0) return(NULL)
    used[,'text'] %>% 
        strip_lint_leads() %>%
        gsub( pattern=attr(used, "pattern")
            , replacement=""
            , perl=TRUE, ignore.case=TRUE) %>%
        structure(used.lines = used, class=c('lint-seealso', 'lint-documentation'))
}
extract_concepts <-
function( pd ){
    #! extract @nomd{@concept}@[s] tag values.
    used <- extract_tagged_lines(pd, "concepts?")
    if(nrow(used)==0) return(NULL)
    used[,'text'] %>% 
        strip_lint_leads() %>%
        gsub( pattern=attr(used, "pattern")
            , replacement=""
            , perl=TRUE, ignore.case=TRUE) %>%
        strsplit(split="\\s+", perl=TRUE) %>% 
        unlist %>%
        structure( used.lines = used
                 , class = c('lint-concepts', 'lint-documentation')
                 )
}

get_valid_keywords <- 
function(){
    #! retrieve the list of valid keywords from the
    #^ installed documentation.
    lines <- readLines(file.path(R.home("doc"), "KEYWORDS"))
    data <- 
    lines[grepl("&", lines)] %>%
        str_trim %>%
        gsub(pat="^& ", rep='') %>% 
        strsplit("\\s*&\\t+") %>%
        unlist() %>%
        matrix(ncol=2, byrow=TRUE) %>%
        as.data.frame() %>%
        structure(names=c("keyword", "description"))
    structure(data[[1]], data=data)
}
extract_keywords <- 
function( pd        
        , .validate = TRUE #< should the keywords specified be validated.
        ){
    #! extract @nomd{@keyword}@[s] tag values.
    used <- extract_tagged_lines(pd, "keywords?")
    if(nrow(used)==0) return(NULL)
    keywords <- used[,'text'] %>% 
        strip_lint_leads() %>%
        gsub( pattern=attr(used, "pattern")
            , replacement=""
            , perl=TRUE, ignore.case=TRUE) %>%
        strsplit(split="(,\\s+|,|\\s+)", perl=TRUE) %>% 
        unlist %>%
        structure( used.lines = used
                 , class = c('lint-keywords', 'lint-documentation')
                 )
    if(length(keywords)==0) return(NULL)
    if(.validate && 
       !all(.i <- keywords %in% get_valid_keywords())){
            warning("Not all keywords (", keywords[.i]
                   , ") were listed in the keywords file,"
                   , file.path(R.home("doc"), "KEYWORDS")
                   , ".  Check your spelling or "
                   , "Consider using a @concept tag instead.")
    }
    return(keywords)
}

get_pd_function_body <- 
function( pd #< `parse-data` for only one.function 
        , id = all_root_nodes(pd)$id #< id of the function root expression.
        ){
    #! Return only the argument portion of the parse data.
    if(is_pd_assignment(pd))
        pd <- get_pd_assign_value(pd)
    stopifnot( is_pd_function(pd) )

    body.id <- pd[pd$parent == id & pd$token=='expr', 'id'] %>% max()
    get_child(body.id, pd=pd)
}

get_pd_return_relative <- function(pd){
    #! Get the Return Value Specified as a Relative Comment.
    if(is_pd_assignment(pd))
        pd <- get_pd_assign_value(pd)
    body.pd <- get_pd_function_body(pd)
    #! Relative placement of a return line comment must be 
    #! the last line of the function body.
    #! Otherwise, use the @nomd{@return}@ tag.
    i <- nrow(body.pd)-1
    while(body.pd[i,'token'] == "CONTINUATION_COMMENT") i <- i-1
    #! Continuation comments are needed if the explanation is
    #! to span more than one line at the end of the body.
    if(body.pd[i,'token'] == "RELATIVE_COMMENT"){
        last.line <- body.pd[i:(nrow(body.pd)-1), ]
        last.line[, 'text'] %>%
            strip_lint_leads() %>%
            paste(collapse=' ') %>%
            structure( class=c( 'lint-return', 'lint-documentation')
                     , used.lines=last.line) %>%
            return
    } else return(NULL)
    #< A text string,
    #^ representing the return value.
}
if(F){#!@testthat
fun <- get_pd_return_relative
pd <- get_parse_data(fun)
rtn <- get_pd_return_relative(pd)
expect_is(rtn, "lint-return")
expect_equal(noattr(rtn), "A text string, representing the return value.")

}
get_return_value <-
function( pd ){
    #! Get the Return Value Documentation.
    #! 
    #! Priority is given to @nomd{@return}@ tagged values
    #! over relatively declared values.  The relative
    #! relative will be overwritten not merged.
    #! In prinicple, the return value for a function
    #! should be documented in one and only one place.
    rtn.lines <- extract_tagged_lines(pd, "return")
    if(nrow(rtn.lines) >1)
        stop( "Documentation Error: multiple @return tags found."
            , "Lines:", paste(rtn.lines$line1, collapse=", "))
    if(nrow(rtn.lines) == 1){
        pattern <- attr(rtn.lines, "pattern")
        rtn.lines <- 
            get_associated_continuation(pd, rtn.lines$id)
        
        rtn.lines$text %>%
        strip_lint_leads() %>%
        str_trim() %>%
        gsub( pattern=pattern
            , replacement=""
            , perl=TRUE, ignore.case=TRUE) %>%
        paste(sep=' ', collapse=' ') %>% 
        structure( used.lines = rtn.lines
                 , class=c('lint-return', 'lint-documentation')
                 ) %>% 
        return()
    } else {
        get_pd_return_relative(pd)
    }
    #! @return a character string of class 'lint-return',
    #^         or NULL if not found
}
if(F){#!@testthat
    pd <- get_parse_data(get_return_value)
    rtn <- get_return_value(pd)
    expect_is(rtn, 'lint-return')
    expect_equal(noattr(rtn), "a character string of class 'lint-return', or NULL if not found")
}

as_person <- function(x){
    if(length(x)>1) sapply(x, as_person)
    if(grepl("^person\\(", x))
        eval(parse(text=x))
    else
        as.person(x)
}
line_to_author <-
function( id  #< id of the comment containing the @nomd{@author}@ tag of interest.
        , pd
        ){
    #! Convert Parse Data to Author/Person contruct.
    #! @author person("Andrew", "Redd"
    #^               , email="andrew.redd@hsc.utah.edu")
    if(length(id)>1){
        authors <- lapply(id, line_to_author, pd=pd)
        used <- lapply(authors, attr, "used.lines") %>%
                Reduce(f=rbind)
        Reduce(c, authors) %>%
        structure(used.lines = used)%>%
        return()
    } else if(length(id)==0){
        return(NULL)
    } else {
        used <- get_associated_continuation(pd, id)
        used$text %>%
            strip_lint_leads() %>%                
            strip_tag("author") %>%
            paste(collapse=" ") %>%
            as_person %>% 
            structure(used.lines = used)
    }
}
if(F){#!@testthat line_to_author
    pd <- get_parse_data(line_to_author)
    id <- extract_tagged_lines(pd, "author")$id
    a <- line_to_author(id, pd)
    expect_identical(a[1], person("Andrew", "Redd", email="andrew.redd@hsc.utah.edu"))
}

extract_author <- 
function(pd){
    aut.lines <- 
        extract_tagged_lines(pd, "author")
    auts.line <- 
        extract_tagged_lines(pd, "authors")
    if(nrow(auts.line) > 0 && nrow(aut.lines) > 0){
        stop("Documentation Error: cannot specify both @author and @authors for the same function.")
    } else if(nrow(aut.lines)>0){
        line_to_author(aut.lines$id,  pd=pd)
    } else if(nrow(auts.line) > 0){
        if(nrow(auts.line) > 1)
            stop("Documentation Error: cannot specify multiple @authors for a single function.")
        auts.line <- 
            get_associated_continuation(pd, auts.line$id)
        docs$author <- 
            auts.line[, 'text'] %>%
            strip_lint_leads() %>%
            strip_tag("authors") %>% 
            paste( collapse=' ') %>%
            strsplit(split=",\\s*", perl=TRUE) %>% 
            unlist %>% as.person %>%
            structure(used.lines = auts.line)
    }
}
if(F){#!@testthat
    test_fun <- function(){
        #! hello world
        #! @author Brian Kernighan(Original C Version)
        #! @author Andrew Redd(reimplimentation in R)
        cat("hello, world")
    }
    pd <- get_parse_data(test_fun)
    authors <- extract_authors(pd)
    expect_is(authors, "person")
    expect_equal(length(authors), 2)
    expect_identical( authors[2], person("Andrew", "Redd", comment="reimplimentation in R"))

    test_fun <- function(){
        #! hello world
        #! @authors Brian Kernighan(Original C Version)
        #^        , Andrew Redd(reimplimentation in R)
        cat("hello, world")
    }
    pd <- get_parse_data(test_fun)
    authors <- extract_authors(pd)
    expect_is(authors, "person")
    expect_equal(length(authors), 2)
    expect_identical( authors[2], person("Andrew", "Redd", comment="reimplimentation in R"))
}

extract_date <- 
function(pd){
    date.lines <- extract_tagged_lines(pd, "date")
    if(nrow(date.lines) < 1) return(NULL)
    if(nrow(date.lines) > 1) 
        stop( "Multiple dates found on lines: "
            ,  paste(date.lines$line1, collapse=", ")
            )
    structure(  date.lines$text %>% 
                strip_lint_leads %>%
                strip_tag("date") %>%
                as.POSIXlt("GMT")
             , used.lines = date.lines
             )
}
if(FALSE){#!@testthat
pd.with.multiple.dates <- 
    parse(text="#! @date 2016-01-27
    #! @date 1776-07-04
    ") %>% 
    get_parse_data
expect_error(extract_date(pd.with.multiple.dates), "Multiple dates found on lines: 1, 2")

pd.with.bad.date <- 
    parse(text="#! @date 1/1/01") %>% 
    get_parse_data()
expect_error(extract_date(pd.with.bad.date), "character string is not in a standard unambiguous format")

pd <- 
    parse(text="#! @date 2016-01-27") %>% 
    get_parse_data
expect_is(extract_date(pd), "POSIXlt")
}


document_function <-
function( fun                         #< a [function] object
        , header.lines = character(0) #< [character]lines that preceed the function in source files
        , ...                         #< arguments to overwrite or append to documentation
        , pd = get_parse_data(fun)    #< parse data from `<get_parse_data>`.
        ){
    #! Document a Function from Source Code
    #! @author person("Andrew", "Redd", email="Andrew.Redd@hsc.utah.edu")
    #! @date 2016-01-13
    #! 
    #! Documents a function with overrides possible specified in the `...` arguments.
    #! See the Details for details of what is inferred and how.
    docs <- function_documentation(...)
    
    #! @details
    if(is.null(docs$name)){
        #! @subsection `name`
        #! Name is taken as the deparsed argument if a single name.
        s <- substitute(fun)
        if(is.name(s))
            docs$name <- deparse(s)
    }
    if(is_vectorized(fun))
        fun <- get("FUN", envir=environment(fun))
    
    if(is_pd_assignment(pd)){
        fname <- subset(get_pd_assign_variable(pd), token=="SYMBOL")$text
        if(length(fname)==1)
            docs$name <- fname
        pd <- get_pd_assign_value(pd)
    }
    
    if(is.null(docs$title)){
        #! @subsection `title`
        #!
        #! The title must be specified as a single character string.
        #! If not specified in `document_function` call see `<get_fun_title>`
        #! for the priority of inference.
        docs$title <- get_fun_title(fun, pd=pd)
    }
    if(is.null(docs$usage)){
        #! @subsection `usage`
        #! usage may be specified as a call, 
        #! but is typically inferred from the 
        #! function definition.
        docs$usage <- make_usage(fun, name=docs$name)
    }
    if(is.null(docs$arguments)){
        #! @subsection `arguments`
        #! arguments must be specified as a `<arguments-list>` object
        docs$arguments <- arguments_list_from_pd(pd=pd)
    }
    #Single lines
    if(is.null(docs$seealso)){
        #! @subsection `seealso`
        #! seealso denotes that everything on that 
        #! line should be included in the seealso section.
        #! Multiple see also tags can be declared in a single function,
        #! they will all be grouped together.
        docs$seealso <- extract_seealso(pd)
    }
    if(is.null(docs$keywords)){
        #! @subsection `keywords`
        #! keywords can be specified with either the 
        #! `@nomd{@keyword}@` or @nomd{@keywords}@
        #! tag, preferably at the beginning of the line 
        #! but technically can be specified at any point 
        #! along the line.
        #! this indicated that all word on the line are 
        #! keywords separated by a space for multiples
        #! there can be many @nomd{@keyword}@ tagged lines that 
        #! will be aggregated.
        docs$keywords <- extract_keywords(pd)
    }
    if(is.null(docs$concepts)){
        #! @subsection `concepts`
        #! Similar to keywords, concepts can be specified 
        #! with either the `@nomd{@concept}@` or `@nomd{@concepts}@`
        #! tag, preferably at the beginning of the line 
        #! but technically can be specified at any point 
        #! along the line.
        #! this indicated that all word on the line are 
        #! concepts (see Writing R Extensions, section 2.9 Indices)
        #! separated by a space for multiples
        #! there can be many @nomd{@keyword}@ tagged lines that 
        #! will be aggregated.
        docs$concepts <- extract_concepts(pd)
    }
    if(is.null(docs$return)){
        #! @subsection `return`
        #! The return value may only be specified once.
        #! It can either be specified by a `@nomd{@return}` tag,
        #! with as many continuation comments as needed.
        #! Or it may be specified as a relative comment 
        #! at the end of the function body, again extended
        #! with as many continuation comments as needed.
        #! Preference is given to the `@nomd{@return}` tags 
        #! and will overwrite relative tags.
        docs$return <- get_return_value(pd)
    }
    if(is.null(docs$author)){
        #! @subsection `@nomd{@author}@`
        #! Authorship can be specified as a string that can 
        #! be converted into a <person> object or through the 
        #! `<person>` construct directly, if roles need to be specified.
        #! 
        #! For ease of interpretation each `@nomd{@author}@` tag specifies 
        #! one and only one author.
        #! the `@nomd{@authors}@` tag with an s does not accept person
        #! entities but will allow a comma separated list of names.
        #! Presence of an @nomd{@author}@ tag overrides any @nomd{@authors}@ tag.
        docs$author <- extract_author(pd)
    }
    if(is.null(docs$date)){
        #! @subsection `@nomd{@date}@`
        #! Date of the function needs to be specified as an unambiguous
        #! date string that can be converted via `<as.POSIXlt>`.
        #! The date is specified with the @nomd{@date}@ tag and only the date present
        #! on the line.
        #! No continuations are allowed.
        docs$date <- extract_date(pd)
    }

    #Multi-Lines separated by `@` tags
    if(is.null(docs$references)){
        #! @subsection References
        #! The references section is specified with a fenced code section 
        #! using three or more of either @nomd{backtick "`" or tilde "~"}@.
        #! see <http://spec.commonmark.org/0.24/#fenced-code-block> for details
        #! of fenced code blocks.
        #! On the opening line the info string must specify @nomd{@references}@.
        #! The content of the code block is yaml coded lists specifying the 
        #! <bibentry>.  There can be only one @nomd{@references}@ section.
        #! 
        #! **Example:**
        #! \code{
        #! @nomd{~~~@references}@ 
        #! - title: lint, Tools to check R code style
        #!   author: Andrew Redd
        #!   year: 2016
        #! @nomd{~~~} 
        #! }
        docs$references <- extract_references(pd)
    }
    
    used <- lapply(docs, attr, "used.lines") %>% Reduce(f=rbind)
    remaining <- pd[ pd$token %in% lint.comments$class
                   & !(pd$id %in% used$id)
                   & !(pd$id %in% only_arguments(pd)$id)
                   ,]
    #TODO document @subsection description
    #TODO document @subsection details
    #! @subsection Description, Details, and Sections
    #! 
    #! Besides the at tags defined above the remaing lines are divided into sections.
    #! The @nomd{@section}@ tag is followed by a title to define an arbitray section.
    #! There are three special sections for R documentation files.  The @nomd{@details}@
    #! tag denotes the details section.  If there are any lines that sequentially
    #! come before the @nomd{@section}@ tag and before any @nomd{@section}@ tags
    #! those lines are subsumed into the description section.
    #! 
    if(nrow(remaining)){
        if(any(remaining$token %in% c("RELATIVE_COMMENT", "CONTINUATION_COMMENT")))
            warning("Unused relative and continuation comments found.")
          
        tags <- "(details|section)"
        tagged.lines <- extract_tagged_lines(remaining, tags)
        
        m <- regexpr(attr(tagged.lines, "pattern"), tagged.lines$text, perl=TRUE, ignore.case=TRUE)
        section.tags <- str_trim(gsub("^@", '', regmatches(tagged.lines$text, m)))
        section.data <- strip_tag(strip_lint_leads(tagged.lines$text), tags)
        has.tag <- remaining$id %in% tagged.lines$id
        sections <- split(remaining, factor(cumsum(has.tag)))
        #TODO subsections
        if(!has.tag[1] && length(sections)>0){
            docs$description <- strip_lint_leads(sections[[1]][['text']])
            while(nchar(head(docs$description,1))==0)docs$description <- tail(docs$description,-1)
            while(nchar(tail(docs$description,1))==0)docs$description <- head(docs$description,-1)
            sections <- sections[-1]
            class(docs$description) <- c( "lint-description"
                                        , "lint-section"
                                        , "lint-documentation"
                                        )
        }
        for(i in seq_along(section.tags)){
            sections[[i]] <- 
                structure( strip_lint_leads(sections[[i]][['text']][-1])
                         , head.line = sections[[i]][['text']][1]
                         , class  = unique(c( sprintf("lint-%s", section.tags[[i]]) 
                                            , 'lint-section'
                                            , 'lint-documentation'
                                            ))
                         )
            while(nchar(head(sections[[i]], 1))==0)sections[[i]] <- tail(sections[[i]],-1)
            while(nchar(tail(sections[[i]], 1))==0)sections[[i]] <- head(sections[[i]],-1)
        }
        if(any(i <- sapply(sections, inherits, 'lint-details'))){
            if (sum(i)>1) stop("multiple details sections defined in function `", docs$name, "`.")
            docs$details <- sections[i][[1]]
            sections <- sections[!i]
        }
        
        
        #TODO: examples
        if(length(sections)){
            for(i in which(sapply(sections, inherits, "lint-section"))){
                sections[[i]] <- 
                    structure( sections[[i]]
                             , section.title = strip_lint_leads(strip_tag(attr(sections[[i]], 'head.line'), "section"))
                             )
            }
        }
        docs$sections <- sections
    }
    for(n in names(docs))attr(docs[[n]], "used.lines") <- NULL     
    
    #! @seealso `<function-documentation>`, `<lint-documentation>`.
    structure( fun
             , class = c("self-documenting-function", class(fun))
             , docs  = docs
             )
    #! @alias    class-self-documenting-function
    #! @keywords programming documentation
    #! @concepts inline-documentation
    #! @return a function of class `self-documenting-function` and attributes docs set.
    #!~~~@references
    #! - title: CommonMark Spec, Version 0.24
    #!   year:  2016
    #!   author: John MacFarlane
    #!   url: http://spec.commonmark.org/0.24/
    #!   bibtype: Manual
    #! - title: "YAML: YAML Ain't Markup Language"
    #!   author: Clark C. Evans
    #!   year: 2016
    #!   url: http://yaml.org/
    #!   bibtype: Manual
    #!~~~
}
if(F){# Development
    rm(list=ls())
    load_all()
    fun <- document_function
    pd <- get_pd_assign_value(get_parse_data(fun))
    docs <- attr(document_function(document_function), 'docs')
    trace(document_function, exit=browser)
    document_function(fun)
    undebug(document_function)
    
    debug(get_fun_title)
    
}
if(F){#! @testthat
fun <- document_function(function(){
    cat("hello world\n")
})
expect_null(attr(fun, "docs")$name)

f <- document_function(arg)
docs <- attr(f, 'docs')

docs <- attr(document_function(document_function), "docs")
expect_equal(docs$title, "Document a Function from Source Code")

fun <- document_function(document_function)
docs <- attr(fun, 'docs')

expect_equal(docs$name, "document_function")
expect_equal(docs$title, "Document a Function from Source Code")
expect_is(docs$usage, 'call')
expect_equal(docs$usage, call("document_function", quote(fun), header.lines = character(0), quote(...)))
# "arguments" 
expect_is(docs$arguments, "arguments-list")
expect_equal(length(docs$arguments), 3)
# "return" 
expect_equal(as.character(docs$return), "a function of class selfDocumentingFunction and attributes docs set.")
# "author"
expect_is(docs$author, "person") 
expect_equal(docs$author, person("Andrew", "Redd", email="Andrew.Redd@hsc.utah.edu"))
# "seealso"   
expect_is(docs$seealso, 'lint-seealso')
# "keywords"  
expect_is(docs$keywords, 'lint-keywords')
expect_equal(length(docs$keywords), 2)
# "concepts" 
expect_is(docs$concepts, 'lint-concepts')
expect_equal(length(docs$concepts), 1)
# details
# sections
# references
# examples

}

`print.self-documenting-function` <- 
function( x, ...){
    #! print a self-documenting-function
    print(attr(x, 'docs'))
    invisible(x)
}

