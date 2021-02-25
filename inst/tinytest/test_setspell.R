# library(tinytest)

# data 
testdata_setspell1 <- data.table::data.table(group=qc(a,a,a,a,b,b,b,c,c,c,c), variabel1=c(1,2,NA,2,1,2,1,1,1,2,2))

# test
# simple test
dtx <- data.table::copy(testdata_setspell1)

setspell(dtx, 'group', 'variabel1')
# sum(dtx$mxspell_group)
expect_true(
	sum(dtx$mxspell_variabel1) ==  15 
)


# test
# simple test 2
dtx <- data.table::copy(testdata_setspell1)
dty <- structure(list(group = c("a", "a", "a", "a", "b", "b", "b", "c", 
"c", "c", "c"), variabel1 = c(1, 2, NA, 2, 1, 2, 1, 1, 1, 2, 
2), run_variabel1 = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 1L, 1L, 2L, 
2L), spell_variabel1 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 
1L, 2L), mxrun_variabel1 = c(4L, 4L, 4L, 4L, 3L, 3L, 3L, 2L, 
2L, 2L, 2L), mxspell_variabel1 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
2L, 2L, 2L, 2L)), row.names = c(NA, -11L), class = c("data.table", 
"data.frame"))
setspell(dtx, 'group', 'variabel1')
expect_equal(
	dtx, dty
)

# test
# testing that setspell_selected correctly identifiend the two 2-values in group A as two different runs, while the two 2-values in group C should be the same run (as there is no other value in between those two)
dtx <- data.table::copy(testdata_setspell1)
setspell_selected(dtx, 'group', 'variabel1', 2)
expect_true(
	sum(dtx$run_variabel1,na.rm=TRUE) ==  6 
)


