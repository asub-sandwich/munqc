test_that("finish labels are normalized from common spellings", {
  expect_equal(.normalize_finish(c("Matte", "MATT", "flat")), rep("matte", 3))
  expect_equal(
    .normalize_finish(c("semi-gloss", "Satin", "SEMI")),
    rep("semigloss", 3)
  )
  expect_equal(
    .normalize_finish(c("Gloss", "glossy", "shiny")),
    rep("gloss", 3)
  )
  expect_true(is.na(.normalize_finish("crinkled")))
})

test_that("chipfinish covers every reference chip exactly once", {
  expect_setequal(chipfinish$chip, unique(colordata$chip))
  expect_false(any(duplicated(chipfinish$chip)))
  expect_true(all(chipfinish$finish %in% FINISHES))
})

test_that("finish is looked up from chipfinish when not supplied", {
  f <- .chip_finish(c("10YR 8/1", "10YR 2/1"))
  expect_equal(f, c("matte", "gloss"))
})

test_that("a user-supplied finish wins over the lookup", {
  f <- .chip_finish(c("10YR 8/1", "10YR 2/1"), c("gloss", NA))
  expect_equal(f, c("gloss", "gloss"))
})

test_that("unknown chips fall back to matte and stay decisive", {
  th <- munqc_thresholds(fail_at = c(matte = 3, gloss = 5))
  expect_equal(.fail_cut(NA_character_, th), 3)
  expect_true(.is_decisive(NA_character_, th))
  expect_true(.is_decisive("nonsense", th))
})

test_that("fail cuts follow finish", {
  th <- munqc_thresholds(fail_at = c(matte = 3, semigloss = 3, gloss = 5))
  expect_equal(.fail_cut(c("matte", "gloss", "semigloss"), th), c(3, 5, 3))
})
