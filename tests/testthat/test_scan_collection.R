test_that("required columns are enforced", {
  expect_error(
    scan_collection(
      data.frame(chip = "10YR 8/1", L = 1, a = 1, b = 1),
      sensor = "nix"
    ),
    "book_id"
  )
  expect_error(
    scan_collection(
      data.frame(book_id = "x", chip = "10YR 8/1", L = 1),
      sensor = "nix"
    ),
    "a, b"
  )
})

test_that("invalid Munsell notation is rejected", {
  expect_error(
    scan_collection(
      data.frame(book_id = "x", chip = "banana", L = 1, a = 1, b = 1),
      sensor = "nix"
    ),
    "Munsell"
  )
})

test_that("sensor, illuminant and observer are validated", {
  d <- perfect_scan()
  expect_error(scan_collection(d, sensor = "flatbed"), "arg")
  expect_error(scan_collection(d, sensor = "nix", illuminant = "D50"), "arg")
  expect_error(scan_collection(d, sensor = "nix", observer = 5), "arg")
})

test_that("observer is stored as an integer", {
  sc <- scan_collection(perfect_scan(), sensor = "nix", observer = 10)
  expect_identical(sc$observer, 10L)
})

test_that("duplicate book/chip pairs warn but are kept", {
  d <- rbind(perfect_scan()[1, ], perfect_scan()[1, ])
  expect_warning(sc <- nix_scan(d), "duplicated")
  expect_equal(nrow(sc$data), 2L)
})

test_that("is_scan_collection discriminates", {
  expect_true(is_scan_collection(nix_scan(perfect_scan())))
  expect_false(is_scan_collection(mtcars))
})

test_that("printing works before and after scoring", {
  sc <- nix_scan(perfect_scan())
  expect_output(print(sc), "No error computed yet")
  expect_output(print(compute_error(sc)), "dE2000")
})
