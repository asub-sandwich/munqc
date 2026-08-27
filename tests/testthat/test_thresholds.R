test_that("defaults fail at dE2000 of 3 for every finish", {
  th <- munqc_thresholds()
  expect_equal(unname(th$fail_at), rep(3, length(FINISHES)))
  expect_equal(th$decisive, "matte")
  expect_length(th$labels, length(th$breaks) + 1L)
})

test_that("malformed bands are rejected", {
  expect_error(munqc_thresholds(breaks = c(3, 1)), "increasing")
  expect_error(munqc_thresholds(breaks = c(-1, 2)), "positive")
  expect_error(munqc_thresholds(breaks = numeric(0)), "non-empty")
  expect_error(munqc_thresholds(labels = c("a", "b")), "must have")
})

test_that("fail_at must land on a band edge", {
  expect_error(munqc_thresholds(fail_at = 2.5), "not among")
  expect_error(
    munqc_thresholds(fail_at = c(matte = 3, gloss = 4)),
    "not among"
  )
  expect_silent(munqc_thresholds(fail_at = c(matte = 3, gloss = 5)))
})

test_that("per-finish fail_at fills in unnamed finishes from matte", {
  th <- munqc_thresholds(fail_at = c(matte = 2, gloss = 5))
  expect_equal(th$fail_at[["matte"]], 2)
  expect_equal(th$fail_at[["semigloss"]], 2)
  expect_equal(th$fail_at[["gloss"]], 5)
})

test_that("named fail_at requires matte and rejects unknown finishes", {
  expect_error(munqc_thresholds(fail_at = c(gloss = 5)), "must include")
  expect_error(munqc_thresholds(fail_at = c(matte = 3, shiny = 5)), "Unknown")
  expect_error(munqc_thresholds(fail_at = c(3, 5)), "single number")
})

test_that("decisive is validated", {
  expect_error(munqc_thresholds(decisive = "sparkly"), "Unknown")
  expect_error(munqc_thresholds(decisive = character(0)), "at least one")
  expect_equal(
    munqc_thresholds(decisive = c("matte", "gloss"))$decisive,
    c("matte", "gloss")
  )
})

test_that("bands are left-closed at every break", {
  g <- .grade(c(0, 0.99, 1, 2.99, 3, 4.99, 5, 99), munqc_thresholds())
  expect_equal(
    as.character(g),
    c(
      "imperceptible",
      "imperceptible",
      "perceptible",
      "acceptable",
      "marginal",
      "marginal",
      "replace",
      "replace"
    )
  )
})

test_that("grades are ordered, so comparisons work", {
  g <- .grade(c(0.5, 4), munqc_thresholds())
  expect_true(is.ordered(g))
  expect_true(g[2] > g[1])
})

test_that("printing does not error", {
  expect_output(print(munqc_thresholds()), "munqc_thresholds")
  expect_output(
    print(munqc_thresholds(fail_at = c(matte = 3, gloss = 5))),
    "advisory only"
  )
})

test_that("a chip exactly on its cut fails", {
  expect_true(.is_fail(3, 3))
  expect_false(.is_fail(3 - 1e-9, 3))
  expect_true(.is_fail(3 + 1e-9, 3))
})

test_that("failing and grading agree at every band edge", {
  th <- munqc_thresholds()
  de <- th$breaks
  fails <- .is_fail(de, .fail_cut(rep("matte", length(de)), th))
  grades <- .grade(de, th)
  # A chip fails exactly when its band is at or past the fail_at band.
  fail_band <- .grade(th$fail_at[["matte"]], th)
  expect_equal(fails, grades >= fail_band)
})

test_that("per-finish cuts are respected at the boundary", {
  th <- munqc_thresholds(fail_at = c(matte = 3, gloss = 5))
  expect_true(.is_fail(3, .fail_cut("matte", th)))
  expect_false(.is_fail(3, .fail_cut("gloss", th)))
  expect_true(.is_fail(5, .fail_cut("gloss", th)))
})
