context('test dups()')

test_that('test failed', {
	a1 <- dups(dupstestdata)
	a2 <- nrow(a1)
	expect_identical(a2, 2L)
} )




