test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("munq_scan from data.frame works", {
  chip = c("10YR 2/2", "10YR 2/1")
  L = c(21, 22)
  a = c(2, 3)
  b = c(3, 4)
  scan <- munq.scan(df, "veykolor")
})
