
###############################################################################
# test.lint.R
# (c)2012 Andrew Redd 
# This is file part of the lint R package, a code style check package for R.
# 
# DESCRIPTION
# -----------
# This file contains the tests for checking the patterns and tests.
# 
# 
################################################################################
context("Testing lint patterns")
library(testthat)
library(lint)
library(parser)
test_that("Testing patterns",{
  file <- 
  check.file <- system.file("examples/checks.R", package="lint")
  lines <- readLines(check.file, encoding="ansi")
  pd <- attr(parser(check.file), 'data')
  
  expect_that(dispatch_test(spacing.linelength.80, file, pd, lines, quiet=T)
    , equals(11:12))
  expect_that(dispatch_test(spacing.linelength.100, file, pd, lines, quiet=T)
    , equals(12))
  expect_that(dispatch_test(spacing.notabs, file, pd, lines, quiet=T)
    , equals(13))
  expect_that(dispatch_test(spacing.indentation.evenindent, file, pd, lines, quiet=T)
    , equals(14))
  expect_that(dispatch_test(spacing.spaceaftercomma, file, pd, lines, quiet=T)
    , equals(17))
  expect_that(dispatch_test(spacing.twobeforecomments, file, pd, lines, quiet=T)
    , equals(23:24))
})
  
test_that("Testing infix operators",{
  infix.file  <- system.file("examples/check-infix.R", package="lint")
  infix.lines <- readLines(infix.file)
  infix.pd    <- attr(parser(infix.file), 'data')
  infix.problems <- dispatch_test(spacing.spacearoundinfix, infix.file
                                  , infix.pd, infix.lines, quiet=T)
  bad.lines <- c(5:12, 15:18, 21:30, 33:40)
  expect_that(infix.problems, equals(bad.lines))
 
  expect_that(dispatch_test(spacing.spacearoundequals, file=infix.file
                , parse.data=infix.pd, lines=infix.lines, quiet=T)
              , equals(91:93))
})
