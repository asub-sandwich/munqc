test_that("hue parses as the page key", {
  expect_equal(.chip_hue("10YR 8/1"), "10YR")
  expect_equal(.chip_hue("7.5YR 2.5/2"), "7.5YR")
  expect_equal(.chip_hue("2.5Y 6/4"), "2.5Y")
})

test_that("value and chroma parse as numbers", {
  expect_equal(.chip_value("7.5YR 2.5/2"), 2.5)
  expect_equal(.chip_chroma("10YR 8/6"), 6)
})

test_that("whitespace is normalized and junk becomes NA", {
  expect_equal(.normalize_chip("  10YR   8/1 "), "10YR 8/1")
  expect_true(is.na(.normalize_chip("not a chip")))
  expect_true(is.na(.normalize_chip("")))
  expect_true(is.na(.normalize_chip("10YR")))
})

test_that("condensing round-trips against parsing", {
  x <- .condense_munsell("7.5YR", "2.5", "2")
  expect_equal(x, "7.5YR 2.5/2")
  expect_equal(.chip_hue(x), "7.5YR")
  expect_equal(.chip_value(x), 2.5)
  expect_equal(.chip_chroma(x), 2)
})
