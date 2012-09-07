
###############################################################################
# test-styles.spacing.R
# (c)2012 Andrew Redd 
# This is file part of the lint R package, a code style check package for R.
# 
# DESCRIPTION
# -----------
# This file contains the tests for checking the patterns and tests.
# 
# 
################################################################################
context("Styles/Spacing")

autotest_style('spacing.linelength.80')
autotest_style('spacing.linelength.100')
autotest_style('spacing.indentation.notabs')
autotest_style('spacing.notabs')
autotest_style('spacing.spacearoundequals')
autotest_style('spacing.spacearoundinfix')
autotest_style('spacing.twobeforecomments')
