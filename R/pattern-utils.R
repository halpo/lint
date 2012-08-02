{############################################################################### 
# spacing.patterns.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# utilities for building and checking style checks.
# 
# LICENSE
# ========
# lint is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################
#' @title Style checks
#' @name stylechecks
#' @docType data
#' 
#' @aliases 
#'     style_checks
#'     style_tests
#' 
#' @format
#' Each test can be defined in several formats and is very flexible.
#' A test consists of a names list of attributes.
#' \enumerate{
#'   \item \code{pattern} is a pcre compatible \link[base:regex]{regular
#'         expression} that is tested. Can be a character vector of expressions.
#'   \item \code{message} The message to be displayed.
#'   \item \code{include.region} lists regions to restrict the search to.
#'         Can be a character vector specifying the known regions, or a list of 
#'         functions that interpret output from \code{\link{parser}}.
#'   \item \code{exclude.region=c('comments', 'string')} lists regions to 
#'         restrict the search to. Operates the sames as \code{include.region}.
#'   \item \code{use.lines=T} should the pattern be evaluated in lines (default)
#'          or as a contiguous block of text.
#'   \item \code{warning=F}
#' }
NULL


#' Test a patten for consistency.
#' 
#' @param check a style check
#' @param ti the test info data
#' @param only.results if true returns results but does not check for
#'                     correspondance.  For debugging.
#' 
#' @return either NULL or throws an error for use with test_that
#' 
test_pattern_style <- function(check, ti, only.results=F) {
    if(is.null(ti$file)) {
        if(is.null(ti$lines)) {
            if(is.null(ti$text))
                stop(paste0("Invalid ti;"
                     , " one of file, lines, or text, must be specified."))
            else {
                ti$file  = textConnection(ti$text)
                ti$lines = textConnection(ti$text)
            }
        } else {
            if(is.null(ti$text)) {
                ti$text = paste(ti$lines, collapse='\n')
                ti$file  = textConnection(ti$text)
            } else {
                lines <- readLines(textConnection(ti$text))
                stopifnot(identical(lines, ti$lines))
                ti$file  = textConnection(ti$text)
            }
        }
    } else {
        if(is.null(ti$lines))
            lines <- readLines(ti$file)
        else 
            stopifnot(identical(ti$lines, readLines(ti$file)))
    }
    on.exit(if(isOpen(ti$file)){close(ti$file)})
    p  <- parser(file=ti$file)
    pd <- 
    parse.data <- attr(p, 'data')
    
    suppressMessages(suppressWarnings(
        results <- dispatch_test(check, file=ti$file, lines=ti$lines
                                , parse.data=parse.data)))
    if(only.results) return(results)
    expect_equivalent(results, ti$results)
    return(invisible(NULL))
}

#' @rdname test_pattern_style
#' @param test.name the name of the test as a string.
#' \code{autotest_style} uses the \code{.testinfo.<<stylename>>} object to 
#' automatically test styles.  The test info object should be a list with 
#' \code{$lines} and \code{$results}. The '\code{$lines}' element is the input 
#' lines and \code{$results} is the find formated data.frame.
#' 
#' @export
autotest_style <- function(test.name) {
test.name <- as.character(substitute(c(test.name)))[-1]
test_that(test.name
    , test_pattern_style( get(test.name)
                        , get(paste0('.testinfo.', test.name))))
}


.no.exclude <- character(0)
escaped.opp <- c('+'='\\+', '*'='\\*', '/'='\\/', '^'='\\^')
nonesc.opp  <- c('-', '<', '>')
base.opp <- c(escaped.opp, nonesc.opp)
extended.opp <- c('\\*\\*')
logical.opp <- c('\\|', '\\|\\|', '&', '&&', '<=', '==', '!=', '>=')
assign.opp  <- c('<-', '->', '<<-', '->>')
special.opp <- c('%[^%]*%')
all.opp    <- c(base.opp, extended.opp, logical.opp, assign.opp, special.opp)
no.lead.rx = "[^\\s!%\\-\\+\\*\\/\\^<>=\\|&]"
any.opp.rx <- paste(all.opp[order(desc(str_length(all.opp)))], collapse='|')



