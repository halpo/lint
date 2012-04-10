
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

test_that("Testing patterns",{
  file <- 
  check.file <- system.file("examples/checks.R", package="lint")
  if (F) {
    lines <- readLines(file, encoding="ansi")
    parse.data <- attr(parser(file), 'data')
  }
  
  expect_that(dispatch_test(spacing.linelength.80, file=check.file)
  , shows_message("found on lines 11, 12"))
  expect_that(dispatch_test(spacing.linelength.100, file=check.file)
  , gives_warning("found on lines 12"))
  expect_that(dispatch_test(spacing.notabs, file=check.file)
  , shows_message("found on lines 13"))
  expect_that(dispatch_test(spacing.indentation.evenindent, file=check.file)
  , shows_message("found on lines 14"))
  expect_that(dispatch_test(spacing.spaceaftercomma, file=check.file)
  , shows_message("found on lines 17"))
  
  infix.file = system.file("examples/check-infix.R", package="lint")
  dispatch_test(spacing.spacearoundinfix, file=infix.file)
})
