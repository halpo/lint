{############################################################################### 
# Conversion.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# functions for conversion between the difference formats
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

#' @name package-lint
#' @title \code{lint}: R code style checker
#' @docType package
#' @author Andrew Redd
#' 
#' @details
#' \code{lint}
#' 
#' @import plyr
#' @import stringr
#' @importFrom parser parser
#' @importFrom harvestr noattr
#' @import foreach
#' @import dostats
#' @include lint.patterns.R
#' @include conversion.R
#' @include family.R
#' @include finders.R
NULL

{ # TODO
  # ----
  # * independent function as a check.
  # * 
  # 
} 

#' Check a pattern against lines
#' 
#' This is part of lint which checks lines of text for stylistic problems.
#' The pattern provided should be a Perl compatible regular expression.
#' 
#' @param lines character vector of lines of text to check, output from 
#'   \code{\link{readLines}}.
#' @param pattern perl compatible regular expression.
#' @param ... discarded.
#' @return returns an integer vector of the problem lines if problems were 
#'   found. otherwise returns TRUE if the lines pass the check. 
#'   \code{\link{isTRUE}} could be useful for checking the return value.
#' @return \link[parse2find]{find} compatible format.
#' @family lint
#' @export
check_pattern <- function(pattern
  , lines
  , ...) {
if(F){
  pattern = "^.{80}\\s*[^\\s]"
}
  if (length(pattern) > 1) {
    foreach(pat=pattern, .combine=merge_find, .multicombine=TRUE
           , .inorder=FALSE) %do% check_pattern(pat, lines, ...)
  } else {
    problem    <- str_locate(pattern=perl(pattern), string=lines)
    locate2find(problem)
  }
}

#' Look for an argument.
#' 
#' @param x an object
#' @param default the default value
#' @return If x is neither NULL nor NA then x otherwise the default
#' @export
with_default <- function(x, default) {
  if (all(is.null(x))) return(default)
  if (length(x) > 0 && all(is.na(x))) return(default)
  x
}


#' Dispatch tests to the appropriate handler
#' @param test the test
#' @param file the file to check
#' @param parse.data parse data from \code{\link{parser}}
#' @param lines the lines to evaluate, overrides file.
#' @param quiet should the test be quiet, i.e. no messages or warnings?
#' @param warning should messages be upgraded to warnings, ignored if 
#'                \code{quiet=TRUE}.
#' 
#' @description
#' runs a test the the appropriate handler.
#' 
#' @return 
#' returns the results from the test handler, which should be either a TRUE for
#' a passed test or the lines, locations, and/or string violating the rules.
#' @export
dispatch_test <- function(test, file, parse.data=attr(parser(file), 'data')
  , lines=readLines(file), quiet=FALSE, warning=with_default(test$warning, FALSE)
) {
  include.region <- with_default(test$include.region, character(0))
  if (length(include.region)>=1L) {
    stop("include.region not yet supported.")
    fun.in.region <- find_fun(include.region)
    in.fe <- foreach(fun=fun.in.region, .combine=rbind.fill, .multicombine=TRUE)
    in.regions <- in.fe %do% fun(file=file, lines=lines, parse.data=parse.data)
  }
  
  exclude.region <- 
    with_default(test$exclude.region, c("find_comment", "find_string"))
  if (length(exclude.region)> 0L) {
    fun.ex.region <- find_fun(exclude.region)
    ex.fe <- foreach(fun=fun.ex.region, .combine=merge_find, .multicombine=TRUE)
    ex.regions <- foreach(fun=fun.ex.region) %do% fun(file=file, lines=lines, parse.data=parse.data)
  }
  
  use.lines = with_default(test$use.lines, TRUE)
  if (!use.lines) lines <- paste(lines, '\n', collapse='')
  
  do_message <- if(quiet){
    function(...){}
  } else if(warning) {
    get("warning", mode="function")
  } else {
    get("message", mode="function")
  }

  if (!is.null(test$pattern)) {
    test.result <- do.call(check_pattern, append(test, list(lines=lines)))
    if(isTRUE(test.result) || nrow(test.result)==0) return(TRUE)
    test.message <- with_default(test$message, test$pattern)
    str <- sprintf("Lint: %s: found on lines %s", test.message, 
                   paste(test.result$line1, collapse=', '))
    do_message(str)
    return(invisible(test.result))
  } else
  stop("Ill-formatted check.")
}
   
#' Check a source document for stylistic errors.
#' @param file a vector of file paths.
#' @param text text to check
#' @param tests The list of tests to use to check.
#' 
#' @family lint
#' @export
lint <- function(file, text=NULL, tests = lint.tests) {
  stopifnot(missing(file)|inherits(file, 'character'))
  if (missing(file) && !is.null(text)) {
    stopifnot(inherits(text, "character"))
    file = textConnection(text)
    on.exit(close(file))
  }
  
  parse.data=attr(parser(file), 'data')
  lines=readLines(file)
  
  llply(lint.tests, dispatch_test, file=file
        , parse.data=parse.data, lines=lines)  
}
