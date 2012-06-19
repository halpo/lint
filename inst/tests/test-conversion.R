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
test_that('parse2find adds 1 to col1 and byte1.', {
    in.pd <- data.frame( line1=1L, col1=0L, byte1=0L
                       , line2=2L, col2=2L, byte2=2L)
    out.f <- data.frame( line1=1L, col1=1L, byte1=1L
                       , line2=2L, col2=2L, byte2=2L)
    expect_that(parse2find(in.pd), equals(out.f))
})
test_that('parse2find collapses data frames.', {
    in.pd <- data.frame( line1=c(1L, 2L), col1=c(0L, 2L), byte1=c(0L, 2L)
                       , line2=c(1L, 3L), col2=c(1L, 3L), byte2=c(1L, 3L))
    out.f <- data.frame( line1=1L, col1=1L, byte1=1L
                       , line2=3L, col2=3L, byte2=3L)
    expect_that(parse2find(in.pd), equals(out.f))
    
})

