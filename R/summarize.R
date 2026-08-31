#' Summarize scored chips up to pages and books
#'
#' Aggregates the chip-level output of [compute_error()] to a page (10YR) or
#' an entire book.
#'
#' @param x A `ScanCollection` that has been through [compute_error()].
#' @param max_fail_frac Proportion of failing decisive chips above which a page
#'   or book is flagged. Defaults to `0.05`.
#'
#' @return A list of four `data.frame`s:
#' \describe{
#'   \item{chip}{One row per scanned chip, as returned by [compute_error()].}
#'   \item{page}{One row per book and Munsell hue.}
#'   \item{finish}{One row per book and surface finish.}
#'   \item{book}{One row per book.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   book_id = "my-book",
#'   chip = c("10YR 8/1", "10YR 8/2", "7.5YR 8/1"),
#'   L = c(81.3, 80.9, 81.2), a = c(1.3, 3.1, 2.4), b = c(6.6, 13.5, 5.6)
#' )
#' sc <- compute_error(scan_collection(df, sensor = "veykolor"))
#' qc_summary(sc)
#'
#' @export
qc_summary <- function(x, max_fail_frac = 0.05) {
  if (!is_scan_collection(x)) {
    stop("`x` must be a ScanCollection.", call. = FALSE)
  }
  if (is.null(x$results)) {
    stop("No results yet. Call compute_error() first.", call. = FALSE)
  }
  if (
    !is.numeric(max_fail_frac) ||
      length(max_fail_frac) != 1L ||
      is.na(max_fail_frac) ||
      max_fail_frac < 0 ||
      max_fail_frac > 1
  ) {
    stop(
      "`max_fail_frac` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }

  res <- x$results

  # Aggregate `rows` over `by`, judging only the decisive chips.
  agg <- function(rows, by, judge = TRUE) {
    parts <- split(rows, rows[by], drop = TRUE, sep = "\r")
    out <- do.call(
      rbind,
      lapply(parts, function(g) {
        d <- g[g$decisive, , drop = FALSE]
        adv <- g[!g$decisive, , drop = FALSE]
        base <- if (judge && nrow(d) > 0L) d else g
        data.frame(
          g[1L, by, drop = FALSE],
          n_chips = nrow(g),
          n_judged = if (judge) nrow(d) else nrow(g),
          n_fail = sum(base$fail),
          fail_frac = if (nrow(base) > 0L) mean(base$fail) else NA_real_,
          median_de = stats::median(base$delta_e_2000),
          p95_de = unname(stats::quantile(base$delta_e_2000, 0.95, type = 7)),
          max_de = max(base$delta_e_2000),
          worst_chip = base$chip[which.max(base$delta_e_2000)],
          n_advisory = if (judge) nrow(adv) else 0L,
          n_advisory_fail = if (judge) sum(adv$fail) else 0L,
          stringsAsFactors = FALSE
        )
      })
    )
    out$flagged <- !is.na(out$fail_frac) &
      (out$fail_frac > max_fail_frac |
        out$p95_de >= min(x$thresholds$fail_at[x$thresholds$decisive]))
    rownames(out) <- NULL
    out[do.call(order, unname(out[by])), , drop = FALSE]
  }

  book <- agg(res, "book_id")
  book$verdict <- ifelse(book$flagged, "replace", "pass")

  finish <- agg(res, c("book_id", "finish"), judge = FALSE)
  finish$decisive <- finish$finish %in% x$thresholds$decisive
  finish <- finish[, setdiff(
    names(finish),
    c("n_judged", "n_advisory", "n_advisory_fail", "flagged")
  )]

  list(
    chip = res,
    page = agg(res, c("book_id", "hue")),
    finish = finish,
    book = book
  )
}

#' @export
summary.ScanCollection <- function(object, ...) {
  out <- qc_summary(object, ...)
  structure(
    c(out, list(thresholds = object$thresholds, sensor = object$sensor)),
    class = "summary.ScanCollection"
  )
}

#' @export
print.summary.ScanCollection <- function(x, ...) {
  th <- x$thresholds
  cat("<ScanCollection QC summary>\n")
  cat("Sensor:   ", x$sensor, "\n", sep = "")
  cat(
    "Decisive: ",
    paste(th$decisive, collapse = ", "),
    "  (other finishes reported separately)\n\n",
    sep = ""
  )

  cat("Grade distribution\n")
  tab <- table(x$chip$grade)
  for (i in seq_along(tab)) {
    cat(sprintf(
      "  %-16s %5d  (%4.1f%%)\n",
      names(tab)[i],
      tab[i],
      100 * tab[i] / sum(tab)
    ))
  }

  cat("\nBy finish\n")
  print(
    x$finish[, c(
      "book_id",
      "finish",
      "n_chips",
      "n_fail",
      "median_de",
      "p95_de",
      "max_de",
      "decisive"
    )],
    row.names = FALSE,
    digits = 3
  )

  cat("\nBy book\n")
  print(
    x$book[, c(
      "book_id",
      "n_judged",
      "n_fail",
      "fail_frac",
      "median_de",
      "p95_de",
      "max_de",
      "n_advisory_fail"
    )],
    row.names = FALSE,
    digits = 3
  )

  pages <- x$page[x$page$n_fail > 0L, , drop = FALSE]
  if (nrow(pages) > 0L) {
    cat("\nPages with failing chips\n")
    print(
      pages[, c(
        "book_id",
        "hue",
        "n_judged",
        "n_fail",
        "fail_frac",
        "median_de",
        "max_de",
        "worst_chip"
      )],
      row.names = FALSE,
      digits = 3
    )
  } else {
    cat("\nNo failing chips on any page.\n")
  }
  invisible(x)
}
