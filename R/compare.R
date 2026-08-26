#' Look up the reference scans and instrument settings for a sensor
#' @noRd
.reference_for <- function(sensor) {
  ref <- colordata[colordata$sensor == sensor, , drop = FALSE]
  if (nrow(ref) == 0L) {
    stop(sprintf("No reference data for sensor \"%s\".", sensor), call. = FALSE)
  }

  meta <- metadata[metadata$sensor == sensor, , drop = FALSE]
  if (nrow(meta) != 1L) {
    stop(
      sprintf(
        "Expected exactly one metadata row for sensor \"%s\", found %d.",
        sensor,
        nrow(meta)
      ),
      call. = FALSE
    )
  }

  list(
    data = ref,
    illuminant = as.character(meta$illuminant),
    observer = as.integer(meta$observer)
  )
}

#' Row-wise CIE DE2000 between two Lab matrices
#'
#' `farver::compare_colour()` returns the full `nrow(from)` x `nrow(to)`
#' distance matrix. We only ever want the diagonal, so this walks the inputs in
#' chunks and keeps memory at O(chunk^2) rather than O(n^2).
#'
#' @noRd
.rowwise_delta_e <- function(from, to, white_from, white_to, chunk = 256L) {
  n <- nrow(from)
  stopifnot(nrow(to) == n)
  out <- numeric(n)

  for (idx in split(seq_len(n), ceiling(seq_len(n) / chunk))) {
    block <- farver::compare_colour(
      from = from[idx, , drop = FALSE],
      to = to[idx, , drop = FALSE],
      from_space = "lab",
      to_space = "lab",
      method = "cie2000",
      white_from = white_from,
      white_to = white_to
    )
    out[idx] <- diag(as.matrix(block))
  }
  out
}

#' Compare scan Lab values against reference Lab values
#' @noRd
.compute_delta_e <- function(
  scan_data,
  ref_data,
  scan_illuminant,
  scan_observer,
  ref_illuminant,
  ref_observer
) {
  # The observer is a property of the colour matching functions,
  # and there is no way to convert Lab measured under the 1964 10 deg
  # observer to the 1931 2 deg observer without the underlying spectra.
  if (!identical(as.integer(scan_observer), as.integer(ref_observer))) {
    stop(
      sprintf(
        paste0(
          "Observer mismatch: your scans use the %d degree observer but the ",
          "\"%s\" reference was measured with the %d degree observer.\n",
          "  These cannot be reconciled from Lab values alone. Re-export your ",
          "scans using the %d degree observer."
        ),
        scan_observer,
        unique(ref_data$sensor),
        ref_observer,
        ref_observer
      ),
      call. = FALSE
    )
  }

  joined <- merge(
    scan_data,
    ref_data[, c("chip", "L", "a", "b")],
    by = "chip",
    suffixes = c("_scan", "_ref"),
    sort = FALSE
  )

  unmatched <- setdiff(unique(scan_data$chip), unique(ref_data$chip))
  if (length(unmatched) > 0L) {
    warning(
      sprintf(
        "%d chip%s in your scans ha%s no reference and %s dropped: %s%s",
        length(unmatched),
        if (length(unmatched) > 1L) "s" else "",
        if (length(unmatched) > 1L) "ve" else "s",
        if (length(unmatched) > 1L) "were" else "was",
        paste(utils::head(unmatched, 5L), collapse = ", "),
        if (length(unmatched) > 5L) ", ..." else ""
      ),
      call. = FALSE
    )
  }
  if (nrow(joined) == 0L) {
    stop("No chips in your scans matched the reference data.", call. = FALSE)
  }

  scan_white <- farver::as_white_ref(scan_illuminant, fow = scan_observer)
  ref_white <- farver::as_white_ref(ref_illuminant, fow = ref_observer)

  scan_lab <- as.matrix(joined[, c("L_scan", "a_scan", "b_scan")])
  ref_lab <- as.matrix(joined[, c("L_ref", "a_ref", "b_ref")])

  if (!identical(toupper(scan_illuminant), toupper(ref_illuminant))) {
    warning(
      sprintf(
        paste0(
          "Your scans use illuminant %s but the reference uses %s. ",
          "Adapting your values to %s.\n",
          "  This is an approximate XYZ-scaling adaptation, not a ",
          "spectral reconstruction; expect some error."
        ),
        toupper(scan_illuminant),
        toupper(ref_illuminant),
        toupper(ref_illuminant)
      ),
      call. = FALSE
    )
    scan_lab <- farver::convert_colour(
      scan_lab,
      from = "lab",
      to = "lab",
      white_from = scan_white,
      white_to = ref_white
    )
    scan_white <- ref_white
  }

  data.frame(
    book_id = joined$book_id,
    chip = joined$chip,
    hue = .chip_hue(joined$chip),
    L = joined$L_scan,
    a = joined$a_scan,
    b = joined$b_scan,
    L_ref = joined$L_ref,
    a_ref = joined$a_ref,
    b_ref = joined$b_ref,
    delta_e_2000 = .rowwise_delta_e(
      scan_lab,
      ref_lab,
      white_from = scan_white,
      white_to = ref_white
    ),
    stringsAsFactors = FALSE
  )
}

#' Score a scan collection against the reference books
#'
#' Computes CIEDE2000 (\eqn{\Delta E_{00}}) between each scanned chip and the
#' corresponding chip in [colordata], then grades it against `thresholds`.
#'
#' @param x A [scan_collection()].
#' @param thresholds A [munq_thresholds()] object. Defaults to failing at
#'   \eqn{\Delta E_{00} \ge 3}.
#'
#' @return `x` with `$results` and `$thresholds` populated. `$results` is a
#'   `data.frame` with one row per scanned chip and columns `book_id`, `chip`,
#'   `hue`, the scanned and reference `L`/`a`/`b`, `delta_e_2000`, `grade`, and
#'   `fail`.
#'
#' @seealso [qc_summary()] for book- and page-level information.
#'
#' @examples
#' df <- data.frame(
#'   book_id = "my-book",
#'   chip = c("10YR 8/1", "10YR 8/2"),
#'   L = c(81.3, 80.9), a = c(1.3, 3.1), b = c(6.6, 13.5)
#' )
#' sc <- scan_collection(df, sensor = "veykolor")
#' compute_error(sc)
#'
#' # Stricter grading
#' compute_error(sc, munq_thresholds(fail_at = 1))
#'
#' @export
compute_error <- function(x, thresholds = munq_thresholds()) {
  if (!is_scan_collection(x)) {
    stop("`x` must be a ScanCollection. See scan_collection().", call. = FALSE)
  }
  if (!is_munq_thresholds(thresholds)) {
    stop("`thresholds` must be a munq_thresholds object.", call. = FALSE)
  }

  ref <- .reference_for(x$sensor)

  results <- .compute_delta_e(
    scan_data = x$data,
    ref_data = ref$data,
    scan_illuminant = x$illuminant,
    scan_observer = x$observer,
    ref_illuminant = ref$illuminant,
    ref_observer = ref$observer
  )

  results$grade <- .grade(results$delta_e_2000, thresholds)
  results$fail <- results$delta_e_2000 >= thresholds$fail_at

  x$results <- results[order(results$book_id, results$hue, results$chip), ]
  rownames(x$results) <- NULL
  x$thresholds <- thresholds
  x
}
