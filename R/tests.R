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


get_test_block <- function(pd){
   
   #! return the testing block if a testing block
   
   if(!identical(pd[1:3,'token'], c('expr', 'IF', '('))) return(NULL)
   if(!( ( pd[4,'token'] == "NUM_CONST" && pd[4,'text'] == "FALSE")
       ||( pd[4,'token'] == "SYMBOL"    && pd[4,'text'] == "F")
       )) return(NULL)

}



extract_testthat <- 
function( file          #< file to extract blocks from
        , pd = NULL     #< Parse data for file
        , dir=file.path(".", "tests", "testthat")  #< directory where to store extracted blocks.
        ){
    #! Extract `if(F){#! @TESTTHAT }` blocks from file
    
    if(is.null(pd)){
        lines <- readLines(file)
        pd <- getParseData(p <- parse(text=lines, keep.source=TRUE))
        pd <- classify_comment( pd )
    }

    
    comments <- extract_tagged_lines(get_lint_comments(pd), "test(that|ing)?")

    lapply(comments$id, get_family, pd=pd, nancestors=2)
    get_family(pd, id=comments$id, nancestors=2)
    testthat.group <- subset(comments, 
        grepl("@testthat\\b", text, ignore.case=TRUE, perl=TRUE)
    )$parent
    
    if(length(testthat.group)==0)return(0)
    
    fam <- get_family(testthat.group, pd)
   
    out.file.name <- file.path(dir, paste0("test-", basename(file)))
    
    out.lines <- with(subset(fam, terminal), tapply(text, line1, paste, collapse=""))
    head.lines <- c( paste("#!", out.file.name)
                   , paste("#!  This file was automatically produced by lint on ", Sys.time())
                   , "#! changes will be overwritten."
                   )
    writeLines(c(head.lines, out.lines), out.file.name)
    return(length(testthat.group))
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
' %>% textConnection() ->file
extract_testthat()
    
    
    
    
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
    lapply(files, extract_testthat, dir=testthat.dir )
}
        
#ENHANCEMENT: run_embedded_tests.

