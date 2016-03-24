{###############################################################################
# test-document_function.R
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
# testing for document_function and associated functions.
# 
# 
}###############################################################################

context("Documentation::function::arg")
test_that("form_argument creates argument", {
    FUN <- form_argument
    srclines <- as.character(attr(FUN, 'srcref'))
    parse.data <- 
    function.parse <- fix_parent(
        getParseData(parsed <- parse(text=srclines))
    )
    
    split.parse <- only_arguments(function.parse)
    
    # there is only one argument so I can pas directly to form argument
    x <- form_argument(split.parse)
    expect_that(x, is_a("argumentDocumentation"))
    expect_that(x$description, equals("parse data for a single argument."))
})
test_that("document_arguments creates argument", {
    FUN <- get_arguments
    srclines <- as.character(attr(FUN, 'srcref'))
    function.parse <- bridge_parent(
        parse.data <-
        getParseData(parsed <- parse(text=srclines))
    )
    x <- document_arguments(function.parse)
    expect_that(x, is_a("argumentsDocumentation"))
    expect_that(x, is_a("refList"))
    expect_that(length(x), equals(2))
})

context("Documentation::function::return")
test_that("get_return_relative", {
    FUN <- split_arguments
    srclines <- as.character(attr(FUN, 'srcref'))
    parse.data <- 
    function.parse <- getParseData(parsed <- parse(text=srclines))
    
    
    
})

test_that("can document a function directly wrapped", {
    hello_world <-
    document_function(function( message=world #< message to run
    ){
        string <- paste("hello", message)
        cat(string)
    })
    expect_that(hello_world@doc$args[1]$name, equals("message"))
})


