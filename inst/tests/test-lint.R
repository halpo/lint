
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
context("Patterns")

test_that('spacing.linelength.80'
    , test_pattern_style( spacing.linelength.80
                        , spacing.linelength.80.testinfo))
test_that('spacing.linelength.100'
    , test_pattern_style( spacing.linelength.100
                        , spacing.linelength.100.testinfo))
test_that('spacing.indentation.notabs'
    , test_pattern_style( spacing.indentation.notabs
                        , spacing.indentation.notabs.testinfo))
test_that('spacing.notabs'
    , test_pattern_style( spacing.notabs
                        , spacing.notabs.testinfo))


# test_that("Testing patterns",{
  # file <- 
  # check.file <- system.file("examples/checks.R", package="lint")
  # lines <- readLines(check.file, encoding="ansi")
  # pd <- attr(parser(check.file), 'data')
  
  
  # expect_that(dispatch_test(spacing.notabs, file, pd, lines, quiet=T)
    # , equals(13))
  # expect_that(dispatch_test(spacing.indentation.evenindent, file, pd, lines, quiet=T)
    # , equals(14))
  # expect_that(dispatch_test(spacing.spaceaftercomma, file, pd, lines, quiet=T)
    # , equals(17))
  # expect_that(dispatch_test(spacing.twobeforecomments, file, pd, lines, quiet=T)
    # , equals(23:24))
# })
  
# test_that("Testing infix operators",{
  # infix.file  <- system.file("examples/check-infix.R", package="lint")
  # infix.lines <- readLines(infix.file)
  # infix.pd    <- attr(parser(infix.file), 'data')
  # infix.problems <- dispatch_test(spacing.spacearoundinfix, infix.file
                                  # , infix.pd, infix.lines, quiet=T)
  # bad.lines <- c(5:12, 15:18, 21:30, 33:40)
  # expect_that(infix.problems, equals(bad.lines))
 
  # expect_that(dispatch_test(spacing.spacearoundequals, file=infix.file
                # , parse.data=infix.pd, lines=infix.lines, quiet=T)
              # , equals(91:93))
# })


