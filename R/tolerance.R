#' Scale factors for mapping Lab onto the munsell page grid
#'
#' Since munsell pages are hues held constant with value on the y-axis
#' and chroma on the x-axis, we can roughly project Lab values onto this
#' plane using L* and C* (chroma calculated from a* and b*). I forgot how I
#' originally calculated these, but I will cite a reference when I remember.
#' @noRd
PAGE_VALUE_SCALE <- 10
PAGE_CHROMA_SCALE <- 6.72

#' Tolerance window around reference chips
#'
#' Creates a window of colors sitting exactly dE away from each
#' reference chip in CIE dE2000. Drawn on a page-space plot, these help
#' visualize the 'window' in which measured chips may fall into and still be
#' recognizable as the original color.
#'
#' @param chips a `data.frame` with columns `chip`, `L`, `a`, `b`. Defaults
#'   to the reference chips for `sensor`.
#' @param de Target distance. Defaults to the matte cut of `thresholds`.
#' @param thresholds A [munqc_thresholds()] object, used only when de is
#'   `NULL`.
#' @param sensor Which reference to use when `chips` is not supplied.
#' @param n Number of points traced around each window. 72 is enough for most
#'   plots.
#' @param iterations Bisection steps. 40 resolves the window radius for plotting
#'   purposes.
#' @param max_radius Largest radius searched, in Lab units.
#'
#' @return a `data.frame` with one row per traced point: `chip`, `L`, `C`, `a`,
#'   `b`, and the page-space coordinates `x` and `y`.
#'
#' @examples
#' w <- tolerance_window(sensor = "veykolor", de = 3, n = 24)
#' head(w)
#'
#' @export
tolerance_window <- function(
  chips = NULL,
  de = NULL,
  thresholds = munqc_thresholds(),
  sensor = SENSORS,
  n = 72L,
  iterations = 40L,
  max_radius = 64
) {
  if (is.null(chips)) {
    sensor <- match.arg(sensor, SENSORS)
    chips <- colordata[colordata$sensor == sensor, c("chip", "L", "a", "b")]
  }
  if (!all(c("chip", "L", "a", "b") %in% names(chips))) {
    stop("`chips` needs columns [chip, L, a, b].", call. = FALSE)
  }
  if (is.null(de)) {
    if (!is_munqc_thresholds(thresholds)) {
      stop("`thresholds` must be a munqc_thresholds object.", call. = FALSE)
    }
    de <- unname(thresholds$fail_at[["matte"]])
  }
  if (!is.numeric(de) || length(de) != 1L || is.na(de) || de <= 0) {
    stop("`de` must be a single positive number.", call. = FALSE)
  }
  n <- as.integer(n)
  if (n < 8L) {
    stop("`n` must be at least 8 for a usable window.", call. = FALSE)
  }

  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  dL <- cos(theta)
  dC <- sin(theta)

  out <- vector("list", nrow(chips))
  for (i in seq_len(nrow(chips))) {
    L0 <- chips$L[i]
    a0 <- chips$a[i]
    b0 <- chips$b[i]
    C0 <- sqrt(a0^2 + b0^2)
    h <- atan2(b0, a0)
    ref <- matrix(c(L0, a0, b0), nrow = 1L)

    lo <- rep(0, n)
    hi <- rep(max_radius, n)
    for (k in seq_len(iterations)) {
      r <- (lo + hi) / 2
      L <- L0 + r * dL
      C <- pmax(C0 + r * dC, 0)
      d <- as.numeric(farver::compare_colour(
        from = cbind(L, C * cos(h), C * sin(h)),
        to = ref,
        from_space = "lab",
        to_space = "lab",
        method = "cie2000"
      ))
      inside <- d < de
      lo <- ifelse(inside, r, lo)
      hi <- ifelse(inside, hi, r)
    }

    r <- (lo + hi) / 2
    L <- L0 + r * dL
    C <- pmax(C0 + r * dC, 0)
    out[[i]] <- data.frame(
      chip = chips$chip[i],
      L = L,
      C = C,
      a = C * cos(h),
      b = C * sin(h),
      x = C / PAGE_CHROMA_SCALE,
      y = L / PAGE_VALUE_SCALE,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Page-space coordinates and colors for a page of chips
#'
#' Patch colors come from converting each reference chip's Lab values
#' directly to sRGB.
#' @noRd
.page_coords <- function(L, a, b) {
  rgb <- farver::convert_colour(cbind(L, a, b), from = "lab", to = "rgb")
  rgb[] <- pmax(pmin(rgb, 255), 0)
  data.frame(
    x = sqrt(a^2 + b^2) / PAGE_CHROMA_SCALE,
    y = L / PAGE_VALUE_SCALE,
    hex = grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}
