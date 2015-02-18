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

extract_testthat <- 
function( file
        , parse.data = NULL
        , dir=file.path(".", "tests", "testthat")
        ){
    #! Extract `if(F){#! @TESTTHAT }` blocks from file
    
    if(is.null(parse.data)){
        lines <- readLines(file)
        parse.data <- getParseData(p <- parse(text=lines, keep.source=TRUE))
        parse.data <- classify_comment( parse.data )
    }

    comments <- subset(parse.data, token == "LINT_COMMENT")
    testthat.group <- subset(comments, 
        grepl("@testthat\\b", text, ignore.case=TRUE, perl=TRUE)
    )$parent
    
    if(length(testthat.group)==0)return(0)
    
    fam <- get_family(testthat.group, parse.data)
   
    out.file.name <- file.path(dir, paste0("test-", basename(file)))
    
    out.lines <- with(subset(fam, terminal), tapply(text, line1, paste, collapse=""))
    head.lines <- c( paste("#!", out.file.name)
                   , paste("#!  This file was automatically produced by lint on ", Sys.time())
                   , "#! changes will be overwritten."
                   )
    writeLines(c(head.lines, out.lines), out.file.name)
    return(length(testthat.group))
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
        


