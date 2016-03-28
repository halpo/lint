{###############################################################################
# extract.R
# This file is part of the R package lint.
#
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
#
# DESCRIPTION
# ===========
# Extract functions to extract derivative files for examples and testing.
#
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# lint is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see http://www.gnu.org/licenses/.
#
}###############################################################################


lint.tests.head.lines <-
c( paste("#! This file was automatically produced by lint on ", Sys.time())
 , "#! changes will be overwritten."
 )

is_Fblock <-
function( pd ){
    #! test if an expresion ID points to a `if(FALSE)` statement.
    #! @keyword internal
    #ENHANCEMENT(Performance):use ID only to allow for non subsetted pd.
    pd <- sort(pd)
    if(!identical(pd[1:3,'token'], c("expr", "IF", "'('"))) return(FALSE)
    return( ( pd[4,'token'] == "NUM_CONST" && pd[4,'text'] == "FALSE")
          ||( pd[4,'token'] == "SYMBOL"    && pd[4,'text'] == "F")
          )
}

filter_if_Fblock_tagged <- function(pd, tag=NULL){
    #! return the testing block if a testing block
    #! @keyword internal
    if(inherits(pd, 'list') && is_parse_data(pd[[1]])){
        l <- lapply(pd, filter_if_Fblock_tagged, tag=tag)
        l <- Filter(Negate(is.null), l)
        return(l)
    }

    pd <- sort(pd)
    if(!is_Fblock(pd)) return(NULL)

    body.id <- subset(pd, token=="'{'")[1,'parent']
    body    <- get_family(body.id, pd)

    info.comment <- get_lint_comments(pd[pd$line1==pd$line1[[1]], ])
    info.string <- if(nrow(info.comment)>0) strip_lint_leads(info.comment$text)
    if(!is.null(tag)) info.string <- strip_tag(info.string, tag)
    info.string <- str_trim(info.string)

    structure( body
             , info.string = info.string
             , tag=tag
             , block.root = pd[1,'id']
             , body.id = body.id
             )
}
extract_block_testthat <-
function( file.in   = NULL  #< file to extract blocks from
        , pd        = NULL  #< Parse data for file, will parse `file.in` if missing.
        , file.out  = NULL  #< file to write tests to, if provided must be fully specified, ie. `dir` will be ignored.
        , dir       = file.path(".", "tests", "testthat")   #< directory where to store extracted blocks.
        ){
    #! Extract `if(F){#! @TESTTHAT }` blocks from file
    tag <- "test(that|ing|s)?"
    if(is.null(file.in)){
        if( is.null(pd) ){
            stop("Either file.in or pd must be provided to extract_block_testthat")
        }
    } else {
        lines <- readLines(file)
        pd <- get_parse_data(p <- parse(text=lines, keep.source=TRUE))
        if(is.null(file.out)){
            file.out <- file.path(dir, sprintf("test-%s", basename(file.in)))
        }
    }
    pd <- sort(pd)
    #! testing blocks can be placed inside the same files as the source
    #! for the functions.  Wrap the lines in curly braces and place
    #! `if(FALSE)` before the opening brace, to denote that the code
    #! should not be run when sourced, such as when building a package.
    #! The `FALSE` may be abbreviated as `F`, but those are the only
    #! two acceptable options.  Also required is a lint comment with a
    #! tag denoting that the block is for testing,
    #! either `@nomd{@testthat}@`, `@nomd{@testing}@`, `@nomd{test}@`,
    #! or simply `@nomd{@test}@` are acceptable.  The comment must be a
    #! lint style comment either `#!` or `#<`, regular comments are
    #! ignored.  The `if` the opening brace `{` and the tag
    #! must all be on the same line.
    #!
    comments <- extract_tagged_lines(get_lint_comments(pd), tag)
    families <- lapply(comments$id, get_family, pd=pd, nancestors=2)
    blocks   <- filter_iff_block_tagged(families, tag)
    if(length(blocks)==0)return(NULL)
    if(!is.null(file.out)){
        cat( lint.tests.head.lines
           , file=file.out, sep='\n', append=FALSE
           )
    }
    for(block in blocks){# Get block associated with
        if(getAttr(block, "string.info", '')==''){
            #! After the `@` tag you may provide an information
            #! string.  At the moment the information string is
            #! only used for two things. First to infer the `desc`
            #! argument of the generated `<test_that>` call.
            #! Second, the information string will be used in the
            #! absence of a provided `file.out` to name the output file,
            #! which will be prefixed by "test-" and placed in the `dir`
            #! directory.
            #!
            #! If the user does not provide the information string
            #! it will be infered as the name of the assignment which
            #! immediately preceeded the block(s).  If the
            #! previous expression was not an assignment
            #! or a similar [example block](extract_block_examples)
            #! the info string, must be provided.
            #!
            test.id <- attr(block, 'block.root')
            prev.ids <- sort(subset(get_family(test.id, pd, 1, 0), id < test.id)$id, TRUE)
            prev.id <- head(prev.ids, 1)
            while(is_Fblock(prev.pd <- get_family(prev.id, pd)) && sum(prev.ids < prev.id)){
                prev.id <- max(prev.ids[prev.ids < prev.id])
            }
            if(is_pd_assignment(prev.pd)){
                attr(block, 'info.string') <-
                    getParseText(prev.pd, get_pd_assign_variable_id(prev.pd))
            } else stop("Malplaced testing block, see the documentation for `extract_block_testthat`.")
        }
        #ENHANCEMENT: extend capabilities of the info string.
        out.text <- sprintf( "test_that(\"%s\", %s)"
                           , attr(block, 'info.string')
                           , getParseText(block, all_root_ids(block))
                           )
        if(is.null(file.out)){
            cat( lint.tests.head.lines
               , out.text, '\n'
               , file = file.path(dir, sprintf("test-%s.r", attr(block, 'info.string')))
               , sep='\n', append=FALSE)
        } else {
            cat( out.text
               , file=file.out
               , sep='\n', append=TRUE)
        }
        #! @return This function is called for the side-effects of
        #^ creating testing files from source files.
        #^ If `file.in` is provided then the file @nomd{"<<dir>>/test-<<file.in>>"}@
        #^ is created, where  @nomd{<<file.in>>}@ is replaced with the value of
        #^ the argument `file.in`, and likewise for @nomd{<<dir>>}@.
        #^ If the `file.in` is not provided. Then the output file is
        #^ generated using the name of the function or object the tests
        #^ correspond to, which is inferred by the location of the
        #^ testthat tagged block.
        #^ This function will overwrite `file.out` if it exists and
        #^ output to write to the file is found.
    }
}
if(FALSE){#! @testthat
'hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}

f2 <- function(){stop("this does nothing")}
if(F){#! @test
    expect_error(f2())
}
if(F){#! example
    hw()
}
' -> text

pd      <- get_parse_data(parse(text=text))
file.in <- textConnection(text)


out <- ''
file.out <- textConnection("out", open="w", local=TRUE)
extract_block_testthat(pd=pd, file.out=file.out)
if(isOpen(file.out)) close(file.out)

expected <- c( "test_that(\"hello_world\", {#!@testthat"
             , "    expect_output(hello_world(), \"hello world\")"
             , "})"
             , "test_that(\"f2\", {#! @test"
             , "    expect_error(f2())"
             , "})"
             )
expect_equal( out[-(1:2)], expected)
}

#' @export
extract_tests <-
function( pkg = '.'
        ){
    #! Extract tests for testing directory.
    stopifnot(file.exists(file.path(pkg, "DESCRIPTION")))
    desc <- read.dcf(file.path(pkg, "DESCRIPTION"))
    desc <- structure(as.list(desc), names=tolower(colnames(desc)))
    desc$path <- normalizePath(pkg)
    for(e in intersect(.T(imports, suggests, depends, collate), names(desc)))
        desc[[e]] <- str_trim(str_split(str_trim(desc[[e]]), "[,\n]+")[[1]])
    stopifnot("testthat" %in% desc$suggests)
    test.dir <- file.path(pkg, "tests")
    if(!file.exists(test.dir)) dir.create(test.dir)
    if(!file.exists(.f <- file.path(test.dir, "testthat.R"))){
        .l <- c('library(testthat)', 'test_check("yourpackage")')
        writeLines(.f, .l)
    }
    if(!file.exists(testthat.dir <- file.path(test.dir, "testthat")))
        dir.create(testthat.dir)
    files <- list.files( file.path(pkg, "R"), pattern="\\.r$"
                       , ignore.case=T, full.names=T)
    lapply(files, extract_block_testthat, dir=testthat.dir )
}

#ENHANCEMENT: run_embedded_tests.

