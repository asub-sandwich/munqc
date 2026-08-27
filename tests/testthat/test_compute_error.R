test_that("a scan identical to the reference has zero error", {
  sc <- compute_error(nix_scan(perfect_scan()))
  expect_true(all(sc$results$delta_e_2000 < 1e-8))
  expect_false(any(sc$results$fail))
})

test_that("results have one row per matched chip, not n squared", {
  d <- perfect_scan()
  sc <- compute_error(nix_scan(d))
  expect_equal(nrow(sc$results), nrow(d))
})

test_that("chunking does not change the answer", {
  d <- perfect_scan()
  d$L <- d$L + seq_len(nrow(d)) / 50
  sc <- compute_error(nix_scan(d))
  ref <- colordata[colordata$sensor == "nix", ]
  m <- match(sc$results$chip, ref$chip)
  w <- farver::as_white_ref("D65", fow = 10)
  direct <- vapply(
    seq_len(nrow(sc$results)),
    function(i) {
      farver::compare_colour(
        matrix(c(sc$results$L[i], sc$results$a[i], sc$results$b[i]), 1),
        matrix(c(ref$L[m[i]], ref$a[m[i]], ref$b[m[i]]), 1),
        from_space = "lab",
        method = "cie2000",
        white_from = w,
        white_to = w
      )[1, 1]
    },
    numeric(1)
  )
  expect_equal(sc$results$delta_e_2000, direct, tolerance = 1e-8)
})

test_that("book_id survives into the results", {
  two <- rbind(perfect_scan("first"), perfect_scan("second"))
  sc <- compute_error(nix_scan(two))
  expect_setequal(unique(sc$results$book_id), c("first", "second"))
  expect_equal(nrow(qc_summary(sc)$book), 2L)
})

test_that("finish is attached to every result row", {
  sc <- compute_error(nix_scan(perfect_scan()))
  expect_true(all(sc$results$finish %in% FINISHES))
  expect_equal(sum(sc$results$finish == "gloss"), 7L)
  expect_equal(sum(sc$results$finish == "semigloss"), 5L)
})

test_that("observer mismatch is refused rather than fudged", {
  expect_error(
    compute_error(
      scan_collection(
        perfect_scan(sensor = "konicaminolta"),
        sensor = "konicaminolta",
        observer = 10
      )
    ),
    "Observer mismatch"
  )
})

test_that("matching observers are accepted", {
  expect_silent(
    compute_error(
      scan_collection(
        perfect_scan(sensor = "konicaminolta"),
        sensor = "konicaminolta",
        observer = 2
      )
    )
  )
})

test_that("unmatched chips warn and are dropped", {
  d <- rbind(
    perfect_scan(),
    data.frame(book_id = "b1", chip = "99XX 1/1", L = 50, a = 0, b = 0)
  )
  expect_warning(sc <- compute_error(nix_scan(d)), "no reference")
  expect_equal(nrow(sc$results), nrow(perfect_scan()))
})

test_that("no overlap at all is an error, not an empty frame", {
  d <- data.frame(book_id = "b1", chip = "99XX 1/1", L = 50, a = 0, b = 0)
  expect_error(
    suppressWarnings(compute_error(nix_scan(d))),
    "No chips"
  )
})

test_that("illuminant conversion warns and changes the values", {
  d <- perfect_scan()
  sc <- scan_collection(d, sensor = "nix", illuminant = "C", observer = 10)
  expect_warning(sc <- compute_error(sc), "Adapting")
  expect_true(max(sc$results$delta_e_2000) > 0)
})

test_that("compute_error validates its arguments", {
  expect_error(compute_error(mtcars), "ScanCollection")
  expect_error(
    compute_error(nix_scan(perfect_scan()), thresholds = 3),
    "munqc_thresholds"
  )
})

test_that("thresholds are stored on the result for downstream use", {
  th <- munqc_thresholds(fail_at = 1)
  sc <- compute_error(nix_scan(perfect_scan()), th)
  expect_identical(sc$thresholds, th)
})
