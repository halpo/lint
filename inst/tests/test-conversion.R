context("Conversion")
source(system.file("examples", "ex-conversion.R", package='lint', mustWork=TRUE))

test_that('locate2find converts to find',{
    expect_that(
        locate2find(l)
        , is_equivalent_to(data.frame(4, 2, 2, 4, 6, 6))
    )
})
test_that('locate2find produces valid find data.',{
    expect_that(valid_find(locate2find(l)), is_true())
})
test_that('merge_find with empty arguments returns empty find.', {
    expect_identical(merge_find(), empty.find)
})
test_that('merge_find with single argument return single argument',{
    expect_that(merge_find(f), is_identical_to(f))
})
test_that('merge_find handles many empty finds.',{
    expect_that(merge_find(empty.find, empty.find), is_identical_to(empty.find))
})


