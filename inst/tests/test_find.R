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
  comment.locations <- {data.frame(  
    line1 = c(2L, 3L),
    col1  = c(3L, 6L),
    byte1 = c(3L, 6L),
    line2 = c(2L, 3L),
    col2  = c(7L, 13L),
    byte2 = c(7L, 13L),
    text  = c('#123', '# there'),
    stringsAsFactors = FALSE)}
  
  pd <- attr(parser(text=text), 'data')
  expect_that((find_comment(pd))[,names(comment.locations)]
    , is_equivalent_to(comment.locations))
  expect_that(strip_comment(lines, parse.data=pd), equals(c('', 'abc', 'hello ', '')))
  expect_that(extract_comment(lines, parse.data=pd), equals(c('', '#123', '# there', '')))
}

test_that("String", {
  text <- {
'"i\'m a string"
a <- "string"
b <- "second
string"
c <- \'c\'
this.line(has=\'two\', "strings")
no.string'}
  lines <- readLines(textConnection(text))
  pd <- attr(parser(text=text), 'data')

  string.loc <- {data.frame(
    line1 = c( 1L,  2L,  3L,  5L,  6L,  6L),
    col1  = c( 0L,  5L,  5L,  5L, 14L, 21L),
    byte1 = c( 0L,  5L,  5L,  5L, 14L, 21L),
    line2 = c( 1L,  2L,  4L,  5L,  6L,  6L),
    col2  = c(14L, 13L,  7L,  8L, 19L, 30L),
    byte2 = c(14L, 13L,  7L,  8L, 19L, 30L),
    text  = c('"i\'m a string"', '"string"', '"second\nstring"', "'c'", "'two'"
             , '"strings"'),
    stringsAsFactors = FALSE)}
  
  expect_that(
    (find_string(pd))[,names(string.loc)]
    , is_equivalent_to(string.loc))
  expect_that(    
      strip_string(lines, parse.data=pd)
    , equals(c('""', 'a <- ""', 'b <- ""', '""', 'c <- ""'
              , 'this.line(has="", "")', "no.string")))
  expect_that(
      extract_string(lines, parse.data=pd)
    , equals(c('""', 'a <- ""', 'b <- ""', '""', 'c <- ""', "no.string")))
    
}

