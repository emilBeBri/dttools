context('test colc()')

test_that('test failed', {
	expect_identical(
		colc(iris, 'sepal', not='length', plus='Species'),
		c('Species', 'Sepal.Width')
	)
} )



# test_that('test af testen', {
# 	expect_identical(
# 		colc(iris, 'sepal', not='length', plus='Species'),
# 		c('Species', 'Sepal.Wasdfidth')
# 	)
# } )






