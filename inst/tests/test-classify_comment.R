
context("classify_comment")
test_that("classify_comment classifies correctly", {
    expect_that(classify_comment("# Regular  "), equals("NORMAL_COMMENT"))
    expect_that(classify_comment("#' Roxygen "), equals("ROXYGEN_COMMENT"))
    expect_that(classify_comment("#! Lint    "), equals("LINT_COMMENT"))
    expect_that(classify_comment("#< Relative"), equals("RELATIVE_COMMENT"))
    expect_that(classify_comment(""), equals(""))
    expect_that(classify_comment(1) , equals(""))
})
test_that("classify_comment vectorizes", {
    comment <- c("# Regular", "#' Roxygen", "#! Lint", "#< Relative")
    classes <- c("NORMAL_COMMENT", "ROXYGEN_COMMENT", "LINT_COMMENT"
                , "RELATIVE_COMMENT")
    expect_that(classify_comment(comment), equals(classes))
})

