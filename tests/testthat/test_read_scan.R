tidy_df <- function() {
  data.frame(
    book_id = "b1",
    chip = c("10YR 8/1", "10YR 8/2"),
    L = c(81.3, 80.9),
    a = c(1.3, 3.1),
    b = c(6.6, 13.5),
    stringsAsFactors = FALSE
  )
}

test_that("a tidy CSV reads with no mapping", {
  sc <- read_scan(write_tmp_csv(tidy_df()), sensor = "nix", observer = 10)
  expect_true(is_scan_collection(sc))
  expect_equal(nrow(sc$data), 2L)
  expect_equal(sc$data$chip, c("10YR 8/1", "10YR 8/2"))
})

test_that("common column-name variants are detected", {
  d <- data.frame(
    Munsell = c("10YR 8/1", "10YR 8/2"),
    `L*` = c(81.3, 80.9),
    `A*` = c(1.3, 3.1),
    `b*` = c(6.6, 13.5),
    check.names = FALSE
  )
  sc <- read_scan(write_tmp_csv(d), sensor = "nix", observer = 10)
  expect_equal(sc$data$L, c(81.3, 80.9))
  expect_equal(sc$data$a, c(1.3, 3.1))
})

test_that("explicit mapping overrides detection", {
  d <- data.frame(
    thing = c("10YR 8/1", "10YR 8/2"),
    one = c(81.3, 80.9),
    two = c(1.3, 3.1),
    three = c(6.6, 13.5)
  )
  sc <- read_scan(
    write_tmp_csv(d),
    sensor = "nix",
    observer = 10,
    chip = "thing",
    L = "one",
    a = "two",
    b = "three"
  )
  expect_equal(sc$data$b, c(6.6, 13.5))
})

test_that("separate hue/value/chroma columns are condensed", {
  d <- data.frame(
    hue = c("10YR", "7.5YR"),
    value = c(8, 2.5),
    chroma = c(1, 2),
    L = c(81.3, 30.1),
    a = c(1.3, 3.1),
    b = c(6.6, 4.5)
  )
  sc <- read_scan(write_tmp_csv(d), sensor = "nix", observer = 10)
  expect_equal(sc$data$chip, c("10YR 8/1", "7.5YR 2.5/2"))
})

test_that("missing Lab columns give an actionable error", {
  d <- data.frame(chip = "10YR 8/1", L = 81.3, a = 1.3)
  expect_error(
    read_scan(write_tmp_csv(d), sensor = "nix"),
    "Could not find Lab column"
  )
})

test_that("unidentifiable chips give an actionable error", {
  d <- data.frame(thing = "10YR 8/1", L = 81.3, a = 1.3, b = 6.6)
  expect_error(
    read_scan(write_tmp_csv(d), sensor = "nix"),
    "Could not identify chips"
  )
})

test_that("ambiguous columns are refused rather than guessed", {
  d <- data.frame(
    chip = "10YR 8/1",
    munsell = "10YR 8/1",
    L = 81.3,
    a = 1.3,
    b = 6.6
  )
  expect_error(read_scan(write_tmp_csv(d), sensor = "nix"), "Ambiguous")
})

test_that("a named column that does not exist is reported", {
  expect_error(
    read_scan(write_tmp_csv(tidy_df()), sensor = "nix", L = "nope"),
    "not in the file"
  )
})

test_that("book_id falls back to the file name", {
  f <- file.path(tempdir(), "site-042.csv")
  utils::write.csv(tidy_df()[, -1], f, row.names = FALSE)
  sc <- read_scan(f, sensor = "nix", observer = 10)
  expect_equal(unique(sc$data$book_id), "site-042")
  unlink(f)
})

test_that("book_id is a literal when it matches no column", {
  d <- tidy_df()[, -1]
  sc <- read_scan(
    write_tmp_csv(d),
    sensor = "nix",
    observer = 10,
    book_id = "drawer-3"
  )
  expect_equal(unique(sc$data$book_id), "drawer-3")
})

test_that("book_id names a column when it matches one", {
  d <- tidy_df()
  names(d)[1] <- "sample"
  sc <- read_scan(
    write_tmp_csv(d),
    sensor = "nix",
    observer = 10,
    book_id = "sample"
  )
  expect_equal(unique(sc$data$book_id), "b1")
})

test_that("several files become several books", {
  f1 <- file.path(tempdir(), "book-a.csv")
  f2 <- file.path(tempdir(), "book-b.csv")
  utils::write.csv(tidy_df()[, -1], f1, row.names = FALSE)
  utils::write.csv(tidy_df()[, -1], f2, row.names = FALSE)
  sc <- read_scan(c(f1, f2), sensor = "nix", observer = 10)
  expect_setequal(unique(sc$data$book_id), c("book-a", "book-b"))
  expect_equal(nrow(sc$data), 4L)
  unlink(c(f1, f2))
})

test_that("a finish column is read and normalized", {
  d <- tidy_df()
  d$Sheen <- c("Glossy", "matt")
  sc <- read_scan(write_tmp_csv(d), sensor = "nix", observer = 10)
  expect_equal(sc$data$finish, c("gloss", "matte"))
})

test_that("a user finish column overrides chipfinish downstream", {
  d <- perfect_scan()
  d$finish <- "gloss"
  sc <- compute_error(nix_scan(d))
  expect_true(all(sc$results$finish == "gloss"))
  expect_false(any(sc$results$decisive))
})

test_that("unreadable Lab rows warn and are dropped", {
  d <- tidy_df()
  d$L <- c("81.3", "not a number")
  expect_warning(
    sc <- read_scan(write_tmp_csv(d), sensor = "nix", observer = 10),
    "unreadable"
  )
  expect_equal(nrow(sc$data), 1L)
})

test_that("missing files and unknown extensions error clearly", {
  expect_error(read_scan("no/such/file.csv", sensor = "nix"), "not found")
  f <- tempfile(fileext = ".docx")
  file.create(f)
  expect_error(read_scan(f, sensor = "nix"), "Don't know how to read")
  unlink(f)
})

test_that("a read scan flows straight into compute_error", {
  ref <- colordata[colordata$sensor == "nix", ]
  f <- write_tmp_csv(data.frame(
    chip = ref$chip,
    L = ref$L,
    a = ref$a,
    b = ref$b
  ))
  sc <- compute_error(read_scan(f, sensor = "nix", observer = 10))
  expect_true(all(sc$results$delta_e_2000 < 1e-8))
  expect_equal(qc_summary(sc)$book$verdict, "pass")
})
