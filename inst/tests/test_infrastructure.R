################################################################################
# test_infrastructure.R
# (c)2012 Andrew Redd 
# This is file part of the lint R package, a code style check package for R.
# 
# DESCRIPTION
# -----------
# This file contains the tests for checking the infrastructure and utilities.
# 
################################################################################
context("Infrastructure")
library(parser)
library(lint)
library(testthat)
test_that("with_default",{
  expect_that(with_default(NULL, T), is_true())
  expect_that(with_default(NULL, F), is_false())
  expect_that(with_default(NA, T), is_true())
  expect_that(with_default(NA, F), is_false())
  expect_that(with_default(NULL, "test"), is_identical_to("test"))
  expect_that(with_default(character(0), "test"), is_identical_to(character(0)))
  expect_that(with_default(T, F), is_true())
  expect_that(with_default(F, T), is_false())
  expect_that(with_default("test", T), is_identical_to("test"))
})
test_that("check_pattern", {
  lines = c(
    "123",
    "abc",
    "xyz")
  expect_that(check_pattern(lines, "no match"), is_true())
  expect_that(check_pattern(lines, "123"), is_equivalent_to(1L))
})
test_that("dispatch_test", {
  file <- 
  check.file <- system.file("examples/checks.R", package="lint")
  parse.data <- attr(parser(check.file), 'data')
  expect_that(
    dispatch_test(list(exclude.region=character(0)), check.file)
  , throws_error("Ill-formatted check."))

  lines = c(
    "123",
    "abc",
    "xyz")
  pd <- attr(parser(text=paste(lines,'\n', collapse='')), 'data')
  expect_that(dispatch_test(list(pattern='abc'), , pd, lines, warning=T)
    , gives_warning('Lint: abc: found on lines 2'))
  expect_that(dispatch_test(list(pattern="\\w{3}"), , pd, lines,  warning=T)
    , gives_warning('Lint: .*: found on lines 1, 2, 3'))
  expect_that(dispatch_test(list(pattern="\\d{3}"), , pd, lines, warning=T)
    , gives_warning('Lint: .*: found on lines 1'))
}) 
