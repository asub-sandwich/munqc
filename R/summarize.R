#' Roll scored chips up to pages and books
#'
#' Aggregates the chip-level output of [compute_error()] to page ("10YR")
#' and book levels.
#'
#' A page or book is flagged when *either* its failure rate exceeds
#' `max_fail_frac` *or* its 95th-percentile chip distance exceeds
#' `thresholds$fail_at`. The first catches books that are uniformly drifting;
#' the second catches books that are fine except for a cluster of ruined chips.
#'
#' @param x A `ScanCollection` that has been through [compute_error()].
#' @param max_fail_frac Proportion of failing chips above which a page or book
#'   is flagged. Defaults to `0.05`.
#'
#' @return A list of three `data.frame`s: `chip`, `page`, and `book`.
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
      max_fail_frac < 0 ||
      max_fail_frac > 1
  ) {
    stop(
      "`max_fail_frac` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }

  res <- x$results
  fail_at <- x$thresholds$fail_at

  agg <- function(by) {
    parts <- split(res, res[by], drop = TRUE, sep = "\r")
    out <- do.call(
      rbind,
      lapply(parts, function(g) {
        data.frame(
          g[1L, by, drop = FALSE],
          n_chips = nrow(g),
          n_fail = sum(g$fail),
          fail_frac = mean(g$fail),
          median_de = stats::median(g$delta_e_2000),
          p95_de = unname(stats::quantile(g$delta_e_2000, 0.95, type = 7)),
          max_de = max(g$delta_e_2000),
          worst_chip = g$chip[which.max(g$delta_e_2000)],
          stringsAsFactors = FALSE
        )
      })
    )
    out$flagged <- out$fail_frac > max_fail_frac | out$p95_de >= fail_at
    rownames(out) <- NULL
    out[do.call(order, unname(out[by])), , drop = FALSE]
  }

  list(
    chip = res,
    page = agg(c("book_id", "hue")),
    book = agg("book_id")
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
  cat(
    "<ScanCollection QC summary>  sensor: ",
    x$sensor,
    "  fail at dE2000 >= ",
    format(x$thresholds$fail_at),
    "\n\n",
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

  cat("\nBy book\n")
  print(x$book, row.names = FALSE)

  flagged <- x$page[x$page$flagged, , drop = FALSE]
  if (nrow(flagged) > 0L) {
    cat("\nFlagged pages\n")
    print(flagged, row.names = FALSE)
  } else {
    cat("\nNo pages flagged.\n")
  }
  invisible(x)
}
