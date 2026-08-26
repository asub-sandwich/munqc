#' Quality-control thresholds
#'
#' Defines the CIEDE2000 (\eqn{\Delta E_{00}}) cut points used to grade colour
#' chips. A single `munq_thresholds` object is shared by [compute_error()],
#' [qc_summary()] and the plotting methods.
#'
#' Because everyone's eyes are different, there is not a single "hard threshold"
#' that determines good from bad. Instead, there are two ways to break up the data.
#'
#' * `breaks` are *descriptive* bands, used for grading and for shading plots.
#' * `fail_at` is the single *decision* cut. A chip is failed when its
#'   \eqn{\Delta E_{00}} is greater than or equal to `fail_at`.
#'
#' The default `fail_at = 3` seems to be a liberal estimate found from many
#' industry sources, and is well above the measurement noise floor
#' observed in [colordata_raw]: across three brand-new books, the median
#' chip-to-reference distance is 0.09-0.23 depending on sensor, and the 95th
#' percentile stays below 1.0. See `vignette("thresholds")` (TODO) for the
#' derivation.
#'
#' @param breaks Increasing, strictly positive numeric vector of band edges.
#' @param labels Character vector naming each band. Must be one longer than
#'   `breaks`.
#' @param fail_at Numeric scalar. Chips at or above this distance are failed.
#'   Must be one of `breaks`, so that the pass/fail line always coincides with
#'   a band edge.
#'
#' @return An object of class `munq_thresholds`.
#'
#' @examples
#' munq_thresholds()
#'
#' # Stricter: fail anything a trained observer could see at all
#' munq_thresholds(fail_at = 1)
#'
#' # Custom bands
#' munq_thresholds(
#'   breaks = c(2, 4),
#'   labels = c("keep", "watch", "replace"),
#'   fail_at = 4
#' )
#'
#' @export
munq_thresholds <- function(
  breaks = c(1, 2, 3, 5),
  labels = c(
    "imperceptible",
    "perceptible by some",
    "acceptable",
    "marginal",
    "replace"
  ),
  fail_at = 3
) {
  if (!is.numeric(breaks) || length(breaks) < 1L || anyNA(breaks)) {
    stop(
      "`breaks` must be a non-empty numeric vector with no missing values.",
      call. = FALSE
    )
  }
  if (any(breaks <= 0)) {
    stop("`breaks` must be strictly positive.", call. = FALSE)
  }
  if (is.unsorted(breaks, strictly = TRUE)) {
    stop("`breaks` must be strictly increasing.", call. = FALSE)
  }
  if (length(labels) != length(breaks) + 1L) {
    stop(
      sprintf(
        "`labels` must have %d elements (one more than `breaks`), not %d.",
        length(breaks) + 1L,
        length(labels)
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(fail_at) || length(fail_at) != 1L || is.na(fail_at)) {
    stop("`fail_at` must be a single non-missing number.", call. = FALSE)
  }
  if (!any(abs(breaks - fail_at) < .Machine$double.eps^0.5)) {
    stop(
      sprintf(
        "`fail_at` (%s) must be one of `breaks` (%s).",
        format(fail_at),
        paste(format(breaks), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      breaks = as.numeric(breaks),
      labels = as.character(labels),
      fail_at = as.numeric(fail_at)
    ),
    class = "munq_thresholds"
  )
}

#' @rdname munq_thresholds
#' @param x A `munq_thresholds` object.
#' @export
is_munq_thresholds <- function(x) {
  inherits(x, "munq_thresholds")
}

#' @rdname munq_thresholds
#' @param ... Ignored.
#' @export
print.munq_thresholds <- function(x, ...) {
  edges <- c(0, x$breaks, Inf)
  band <- sprintf(
    "[%s, %s)",
    format(edges[-length(edges)], trim = TRUE),
    format(edges[-1L], trim = TRUE)
  )
  verdict <- ifelse(edges[-length(edges)] >= x$fail_at, "FAIL", "pass")

  cat("<munq_thresholds>\n")
  cat("Fail at dE2000 >=", format(x$fail_at), "\n\n")
  cat(
    paste0(
      "  ",
      formatC(x$labels, width = -max(nchar(x$labels))),
      "  ",
      formatC(band, width = -max(nchar(band))),
      "  ",
      verdict,
      collapse = "\n"
    ),
    "\n"
  )
  invisible(x)
}

#' Assign band labels to distances
#'
#' @param delta_e Numeric vector of CIEDE2000 distances.
#' @param thresholds A `munq_thresholds` object.
#' @return A factor with levels `thresholds$labels`.
#' @noRd
.grade <- function(delta_e, thresholds) {
  cut(
    delta_e,
    breaks = c(-Inf, thresholds$breaks, Inf),
    labels = thresholds$labels,
    right = FALSE,
    ordered_result = TRUE
  )
}
