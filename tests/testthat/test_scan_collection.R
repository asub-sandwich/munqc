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

.three_books <- function() {
  ref <- colordata[colordata$sensor == "nix", ]
  mk <- function(id) {
    data.frame(book_id = id, chip = ref$chip, L = ref$L, a = ref$a, b = ref$b)
  }
  scan_collection(
    rbind(mk("A"), mk("B"), mk("C")),
    sensor = "nix",
    illuminant = "D65",
    observer = 10
  )
}

test_that("book_ids lists what is present", {
  expect_equal(book_ids(.three_books()), c("A", "B", "C"))
  expect_error(book_ids(mtcars), "scan collection")
})

test_that("selecting one book returns only that book", {
  sc <- books(.three_books(), "A")
  expect_equal(book_ids(sc), "A")
  expect_equal(nrow(sc$data), 102L)
})

test_that("selecting several books returns exactly those", {
  sc <- books(.three_books(), c("A", "C"))
  expect_equal(book_ids(sc), c("A", "C"))
  expect_equal(nrow(sc$data), 204L)
})

test_that("the result is still a ScanCollection and still scores", {
  sc <- books(.three_books(), "B")
  expect_true(is_scan_collection(sc))
  expect_equal(sc$sensor, "nix")
  expect_equal(sc$observer, 10L)
  expect_equal(nrow(qc_summary(compute_error(sc))$book), 1L)
})

test_that("negate inverts the selection", {
  sc <- books(.three_books(), "A", negate = TRUE)
  expect_equal(book_ids(sc), c("B", "C"))
})

test_that("a partly unknown selection warns and keeps only real books", {
  expect_warning(sc <- books(.three_books(), c("A", "zzz")), "No such book")
  expect_equal(book_ids(sc), "A")
  expect_equal(nrow(sc$data), 102L)
})

test_that("an entirely unknown selection errors rather than returning nothing", {
  expect_error(
    suppressWarnings(books(.three_books(), "zzz")),
    "No books left"
  )
  expect_error(
    books(.three_books(), c("A", "B", "C"), negate = TRUE),
    "No books left"
  )
})

test_that("arguments are validated", {
  expect_error(books(mtcars, "A"), "scan collection")
  expect_error(books(.three_books()), "at least one book")
  expect_error(books(.three_books(), character(0)), "at least one book")
  expect_error(books(.three_books(), "A", negate = NA), "TRUE or FALSE")
})

test_that("results are subset alongside data", {
  sc <- compute_error(.three_books())
  sub <- books(sc, "A")
  expect_setequal(unique(sub$results$book_id), "A")
  expect_equal(nrow(sub$results), 102L)
  expect_setequal(unique(sub$data$book_id), unique(sub$results$book_id))
})

test_that("subsetting after scoring matches scoring after subsetting", {
  all_first <- books(compute_error(.three_books()), "B")$results
  sub_first <- compute_error(books(.three_books(), "B"))$results
  rownames(all_first) <- NULL
  rownames(sub_first) <- NULL
  expect_equal(all_first, sub_first)
})

test_that("numeric book ids are matched as character", {
  ref <- colordata[colordata$sensor == "nix", ][1:3, ]
  d <- rbind(
    data.frame(book_id = 1, chip = ref$chip, L = ref$L, a = ref$a, b = ref$b),
    data.frame(book_id = 2, chip = ref$chip, L = ref$L, a = ref$a, b = ref$b)
  )
  sc <- scan_collection(d, sensor = "nix", observer = 10)
  expect_equal(book_ids(books(sc, 1)), "1")
})
