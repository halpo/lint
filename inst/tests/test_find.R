###############################################################################
# test_find.R
# (c)2012 Andrew Redd 
# This is file part of the lint R package, a code style check package for R.
# 
# DESCRIPTION
# -----------
# This file contains the unit tests for the find, strip, extract functions.

# 
# 
################################################################################
library(testthat)
library(lint)
context("find/strip/extract")

test_that("Comments"), {
text={"
abc#123
hello # there
"}
  lines <- readLines(textConnection(text))
  comment.locations <- data.frame(  
    line1 = c(2L, 3L),
    col1  = c(3L, 6L),
    byte1 = c(3L, 6L),
    line2 = c(2L, 3L),
    col2  = c(7L, 13L),
    byte2 = c(7L, 13L),
    text  = c('#123', '# there'),
    stringsAsFactors = FALSE)
  
  pd <- attr(parser(text=text), 'data')
  expect_that((find_comment(pd))[,names(comment.locations)]
    , is_equivalent_to(comment.locations))
  expect_that(strip_comment(lines), equals(c('', 'abc', 'hello ', '')))
  expect_that(extract_comment(lines), equals(c('', '#123', '# there', '')))
}



