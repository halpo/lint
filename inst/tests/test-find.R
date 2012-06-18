{############################################################################### 
# test_find.R
# This file is part of the R package lint.
# 
# Copyright 2012 Andrew Redd
# Date: 6/16/2012
# 
# DESCRIPTION
# ===========
# This file contains the unit tests for the find, strip, extract functions.
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
library(testthat)
library(lint)
context("find/strip/extract")

test_that("Find comments", {
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
  expect_that(find_comment(pd)[,names(comment.locations)]
    , is_equivalent_to(comment.locations))
})
test_that("Find strings", {
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
  # results not yet defined
  # expect_that(
      # extract_string(lines, parse.data=pd)
    # , equals(c('""', 'a <- ""', 'b <- ""', '""', 'c <- ""', "no.string")))
    
})
test_that("Find functions arguments", {
  {text <- 
'function(){}                      # empty   
function(){cat("Hello World!\\n")}  # with body
function()cat("Hello World!\\n")    # single line function
function()
  cat("Hello World!\\n")            # single line on other line
a <- function(){}                  # assignment

# Arguments              
a <- function(file){cat("Hello World!\\n", file=file)} 
a <- function(file, b=2){cat("Hello World!\\n", file=file)} 

# Multiline arguments
a <- function(file,  
              b=2,
              ...) {
                cat("Hello World!\\n", file=file)
              }
              
# embeded              
llply(1:9, function(x){x ** 0.5})
'}
  {lines.no.args <- readLines(textConnection(
'function(){}                      # empty   
function(){cat("Hello World!\\n")}  # with body
function()cat("Hello World!\\n")    # single line function
function()
  cat("Hello World!\\n")            # single line on other line
a <- function(){}                  # assignment

# Arguments              
a <- function(){cat("Hello World!\\n", file=file)} 
a <- function(){cat("Hello World!\\n", file=file)} 

# Multiline arguments
a <- function(

) {
                cat("Hello World!\\n", file=file)
              }
              
# embeded              
llply(1:9, function(){x ** 0.5})
'))}
  {int.loc <- data.frame(
      line1 = as.integer(c( 9, 10, 13, 20))
    , col1  = as.integer(c(14, 14, 14, 20))
    , byte1 = as.integer(c(14, 14, 14, 20))
    , line2 = as.integer(c( 9, 10, 15, 20))
    , col2  = as.integer(c(18, 23, 17, 21))
    , byte2 = as.integer(c(18, 23, 17, 21))
    , stringsAsFactors = FALSE)}
  {full.loc <- data.frame(
      line1 = as.integer(c( 1,  2,  3,  4,  6,  9, 10, 13, 20))
    , col1  = as.integer(c( 0,  0,  0,  0,  5,  5,  5,  5, 11))
    , byte1 = as.integer(c( 0,  0,  0,  0,  5,  5,  5,  5, 11))
    , line2 = as.integer(c( 1,  2,  3,  4,  6,  9, 10, 15, 20))
    , col2  = as.integer(c(10, 10, 10, 10, 15, 19, 24, 18, 22))
    , byte2 = as.integer(c(10, 10, 10, 10, 15, 19, 24, 18, 22))
    , stringsAsFactors = FALSE)}
  lines <- readLines(textConnection(text))
  pd <- attr(parser(text=text), 'data')
  expect_that(  
      find_function_args(pd, internal=TRUE)[,names(int.loc)]
    , equals(int.loc))
  expect_that(  
      find_function_args(pd, internal=FALSE)[,names(full.loc)]
    , equals(full.loc))
  # results not defined
  # expect_that(strip_function_args(lines, parse.data=pd),
    # , equals(lines.no.args))
})
