test_that("transparency works", {
  cc <- as.character(trans.col('red', 0.6))
  expect_equal(cc, '#FF000099')
})

test_that("color vecrtor works", {
  expect_equal(length(trans.col(rep('red', 4), 0.6)), 4)
})
