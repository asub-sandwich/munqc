test_that("a clean book passes", {
  s <- qc_summary(compute_error(nix_scan(perfect_scan())))
  expect_equal(s$book$verdict, "pass")
  expect_false(any(s$page$flagged))
})

test_that("a drifted page is flagged, and only that page", {
  sc <- compute_error(nix_scan(nudge_hue(perfect_scan(), "10YR")))
  p <- qc_summary(sc)$page
  expect_true(p$flagged[p$hue == "10YR"])
  expect_false(any(p$flagged[p$hue != "10YR"]))
})

test_that("a book bad only on glossy chips is NOT condemned", {
  d <- nudge_finish(perfect_scan(), "gloss", dL = 20)
  s <- qc_summary(compute_error(nix_scan(d)))

  expect_equal(s$book$verdict, "pass")
  expect_true(s$book$n_advisory_fail > 0)
  expect_equal(s$book$n_fail, 0)
})

test_that("the same book IS condemned when gloss is made decisive", {
  d <- nudge_finish(perfect_scan(), "gloss", dL = 20)
  th <- munqc_thresholds(decisive = c("matte", "gloss"))
  s <- qc_summary(compute_error(nix_scan(d), th))
  expect_equal(s$book$verdict, "replace")
})

test_that("a book bad on matte chips IS condemned", {
  d <- nudge_finish(perfect_scan(), "matte", dL = 20)
  s <- qc_summary(compute_error(nix_scan(d)))
  expect_equal(s$book$verdict, "replace")
})

test_that("advisory failures are counted but excluded from fail_frac", {
  d <- nudge_finish(perfect_scan(), "gloss", dL = 20)
  s <- qc_summary(compute_error(nix_scan(d)))
  expect_equal(s$book$n_judged, 90L)
  expect_equal(s$book$n_advisory, 12L)
  expect_equal(s$book$fail_frac, 0)
})

test_that("the finish table reports every finish present", {
  s <- qc_summary(compute_error(nix_scan(perfect_scan())))
  expect_setequal(s$finish$finish, FINISHES)
  expect_equal(s$finish$decisive, s$finish$finish == "matte")
  expect_equal(sum(s$finish$n_chips), 102L)
})

test_that("a loose gloss cut spares chips a tight one would fail", {
  d <- nudge_finish(perfect_scan(), "gloss", dL = 4)
  tight <- compute_error(nix_scan(d), munqc_thresholds(fail_at = 3))
  loose <- compute_error(
    nix_scan(d),
    munqc_thresholds(fail_at = c(matte = 3, gloss = 5))
  )
  expect_true(sum(tight$results$fail) > sum(loose$results$fail))
})

test_that("max_fail_frac is validated", {
  sc <- compute_error(nix_scan(perfect_scan()))
  expect_error(qc_summary(sc, max_fail_frac = 2), "between 0 and 1")
  expect_error(qc_summary(sc, max_fail_frac = "a"), "between 0 and 1")
})

test_that("qc_summary requires results", {
  expect_error(qc_summary(nix_scan(perfect_scan())), "compute_error")
  expect_error(qc_summary(mtcars), "ScanCollection")
})

test_that("summary printing works for both verdicts", {
  clean <- compute_error(nix_scan(perfect_scan()))
  expect_output(print(summary(clean)), "pass")

  bad <- compute_error(nix_scan(nudge_finish(perfect_scan(), "matte", 20)))
  expect_output(print(summary(bad)), "replace")
})

test_that("gloss-only damage prints the advisory note", {
  d <- nudge_finish(perfect_scan(), "gloss", dL = 20)
  expect_output(
    print(summary(compute_error(nix_scan(d)))),
    "did not affect the verdict"
  )
})
