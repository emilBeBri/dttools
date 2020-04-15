context('test dups()')

test_that('dups giver det forventede output', {
	a1 <- dups(dupstestdata)
	a2 <- nrow(a1)
	expect_identical(a2, 2L)
} )


