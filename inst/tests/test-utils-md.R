{###############################################################################
# test-utils-md.R
# 
# 
# 
}###############################################################################

context("utils::md")

test_that("md2rd_code", {
    string <- "`test`"
    r <- md2rd_code(string)
    expect_that(r, equals("\\code{test}"))

    string <- "one `two` three `four`"
    r <- md2rd_code(string)
    expect_that(r, equals("one \\code{two} three \\code{four}"))
    
    string <- "one `two` three `"
    r <- md2rd_code(string)
    expect_that(r, equals("one \\code{two} three `"))
    
    string <- "one \\`two` three"
    r <- md2rd_code(string)
    expect_that(r, equals("one \\`two` three"))
})
test_that("md2rd_link", {
    string <- "<rnorm>"
    r <- md2rd_link(string)
    expect_that(r, equals("\\link{rnorm}"))
    
    string <- "<Random Normal>(rnorm)"
    r <- md2rd_link(string)
    expect_that(r, equals("\\link[=rnorm]{Random Normal}"))    
})
test_that("md2rd nested", {
    string <- "`<rnorm>`"
    r <- md2rd(string)
    expect_that(r, equals("\\code{\\link{rnorm}}"))

    string <- "`<Normal>(rnorm)`"
    r <- md2rd(string)
    expect_that(r, equals("\\code{\\link[=rnorm]{Normal}}"))


})
