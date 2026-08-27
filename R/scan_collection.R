SENSORS <- c("veykolor", "nix", "colormuse", "konicaminolta")
ILLUMINANTS <- c("D65", "C")
OBSERVERS <- c("10", "2")

#' Create a collection of colour-book scans
#'
#' Wraps a `data.frame` of measured chip colours together with the
#' instrument settings needed to compare them against [colordata].
#'
#' @param data A `data.frame` (or coercible) with columns `book_id`, `chip`,
#'   `L`, `a`, and `b`. `chip` holds munsell notation like
#'   `"10YR 8/1"`. Extra columns are carried through.
#' @param sensor Which colorimeter produced the readings. Must be one of
#'   `r paste0('"', SENSORS, '"', collapse = ", ")`.
#' @param illuminant Standard illuminant the instrument reported against.
#' @param observer CIE standard observer, `10` or `2` degrees.
#'
#' @return An object of class `ScanCollection`.
#'
#' @examples
#' df <- data.frame(
#'   book_id = "my-book",
#'   chip = c("10YR 8/1", "10YR 8/2", "7.5YR 8/1"),
#'   L = c(81.3, 80.9, 81.2),
#'   a = c(1.3, 3.1, 2.4),
#'   b = c(6.6, 13.5, 5.6)
#' )
#' scan_collection(df, sensor = "veykolor")
#'
#' @export
scan_collection <- function(
  data,
  sensor = SENSORS,
  illuminant = ILLUMINANTS,
  observer = OBSERVERS
) {
  sensor <- match.arg(sensor, SENSORS)
  illuminant <- match.arg(illuminant, ILLUMINANTS)
  observer <- as.integer(match.arg(as.character(observer), OBSERVERS))

  if (!is.data.frame(data)) {
    data <- tryCatch(
      as.data.frame(data),
      error = function(e) {
        stop("`data` could not be coerced to a data.frame.", call. = FALSE)
      }
    )
  }

  required <- c("book_id", "chip", "L", "a", "b")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "`data` is missing required column%s: %s.",
        if (length(missing) > 1L) "s" else "",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data$book_id <- as.character(data$book_id)
  data$chip <- .normalize_chip(data$chip)
  for (nm in c("L", "a", "b")) {
    data[[nm]] <- as.numeric(data[[nm]])
  }

  if (anyNA(data$chip)) {
    stop(
      "`data$chip` contains values that are not valid Munsell notation ",
      "(expected e.g. \"10YR 8/1\").",
      call. = FALSE
    )
  }

  dup <- duplicated(data[, c("book_id", "chip")])
  if (any(dup)) {
    warning(
      sprintf(
        "%d duplicated book_id/chip pair%s in `data`; all rows retained.",
        sum(dup),
        if (sum(dup) > 1L) "s" else ""
      ),
      call. = FALSE
    )
  }

  new_scan_collection(
    data = data,
    sensor = sensor,
    illuminant = illuminant,
    observer = observer
  )
}

#' Low-level `ScanCollection` constructor
#'
#' Performs no validation. Use [scan_collection()] instead.
#'
#' @inheritParams scan_collection
#' @param results Chip-level results from [compute_error()], or `NULL`.
#' @param thresholds A [munqc_thresholds()] object, or `NULL`.
#' @noRd
new_scan_collection <- function(
  data,
  sensor,
  illuminant,
  observer,
  results = NULL,
  thresholds = NULL
) {
  structure(
    list(
      data = data,
      sensor = sensor,
      illuminant = illuminant,
      observer = observer,
      results = results,
      thresholds = thresholds
    ),
    class = "ScanCollection"
  )
}

#' Test whether an object is a `ScanCollection`
#'
#' @param x An object.
#' @return `TRUE` or `FALSE`.
#' @examples
#' is_scan_collection(1:10)
#' @export
is_scan_collection <- function(x) {
  inherits(x, "ScanCollection")
}

#' @export
print.ScanCollection <- function(x, ...) {
  cat("<ScanCollection>\n")
  cat("Sensor:     ", x$sensor, "\n", sep = "")
  cat("Illuminant: ", x$illuminant, "\n", sep = "")
  cat("Observer:   ", x$observer, " degree\n", sep = "")
  cat("Books:      ", length(unique(x$data$book_id)), "\n", sep = "")
  cat(
    "Chips:      ",
    length(unique(x$data$chip)),
    " unique (",
    nrow(x$data),
    " rows)\n",
    sep = ""
  )

  if (is.null(x$results)) {
    cat("\nNo error computed yet. Call compute_error().\n")
  } else {
    n <- nrow(x$results)
    n_fail <- sum(x$results$fail, na.rm = TRUE)
    cat(
      "\ndE2000: median ",
      format(round(stats::median(x$results$delta_e_2000, na.rm = TRUE), 2)),
      ", max ",
      format(round(max(x$results$delta_e_2000, na.rm = TRUE), 2)),
      "\n",
      sep = ""
    )
    cat(
      "Failing (dE >= ",
      format(x$thresholds$fail_at),
      "): ",
      n_fail,
      "/",
      n,
      " chips (",
      format(round(100 * n_fail / n, 1)),
      "%)\n",
      sep = ""
    )
  }
  invisible(x)
}
