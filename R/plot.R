#' Default fill colours for threshold bands
#'
#' Green through red, interpolated to however many bands the user defined.
#' @noRd
.band_palette <- function(n) {
  grDevices::hcl.colors(n, palette = "hawaii", rev = TRUE)
}

#' Rectangles describing each threshold band, for shading plots
#' @noRd
.band_frame <- function(thresholds, upper) {
  edges <- c(0, thresholds$breaks)
  data.frame(
    xmin = edges,
    xmax = c(thresholds$breaks, max(upper, max(thresholds$breaks) * 1.1)),
    grade = factor(thresholds$labels, levels = thresholds$labels),
    stringsAsFactors = FALSE
  )
}

#' Plot a scored scan collection
#'
#' @param x A `ScanCollection` that has been through [compute_error()].
#' @param type One of `"histogram"`, `"page"`, `"page_space"`.
#'   Defaults to "`histogram`".
#' @param binwidth Histogram bin width. Defaults to `0.25`.
#' @param scale The scale of the dE axis, one of `"log"`, `"linear"`. Defaults
#'   to `"log"`.
#' @param facet_books Facet by `book_id` when more than one book is present.
#'   Defaults to `TRUE`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' sc <- compute_error(scan_collection(my_scan_data, sensor = "nix"))
#' plot(sc)
#' plot(sc, binwidth = 0.3)
#' plot(sc, type = "page")
#' plot(sc, type = "page_space", facet_books = TRUE)
#' }
#'
#' @export
plot.ScanCollection <- function(
  x,
  type = c("histogram", "page", "page_space"),
  binwidth = 0.25,
  scale = c("log", "linear"),
  facet_books = TRUE,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" is required for plotting. Install with",
      "install.packages(\"ggplot2\")",
      call. = FALSE
    )
  }
  if (!is_scan_collection(x)) {
    stop("`x` must be a ScanCollection.", call. = FALSE)
  }
  if (is.null(x$results)) {
    stop(
      "Errors and results have not been calculated. Call compute_error() first.",
      call. = FALSE
    )
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  res <- x$results

  if (scale == "log" && any(res$delta_e_2000 <= 0, na.rm = TRUE)) {
    warning(
      sprintf(
        "%d chip(s) matched the reference exactly and cannot be shown on a log axis; using linear axis.",
        sum(res$delta_e_2000 <= 0, na.rm = TRUE)
      ),
      call. = FALSE
    )
    scale <- "linear"
  }

  log_x <- if (scale == "log") {
    ggplot2::scale_x_log10(
      breaks = c(0.25, 0.5, 1, 2, 4, 6, 10, 20),
      labels = c("0.25", "0.5", "1", "2", "4", "6", "10", "20")
    )
  } else {
    NULL
  }
}

#' Plot a munsell-esque page of chips
#'
#' Models a munsell-esque page with chroma increasing along the x-axis
#' and value increasing along the y-axis. Also wraps the center of each
#' color in a tolerance window.
#' @noRd
.plot_page_space <- function(x, res, bands = NULL, facet_books = TRUE) {
  th <- x$thresholds
  ref <- colordata[colordata$sensor == x$sensor, c("chip", "L", "a", "b")]
  ref <- ref[ref$chip %in% res$chip, , drop = FALSE]

  cuts <- sort(unique(.fail_cut(res$finish, th)))
  windows <- do.call(
    rbind,
    lapply(cuts, function(cut) {
      w <- tolerance_window(ref, de = cut, n = 72L)
      w$cut <- factor(format(cut), levels = format(cuts))
      w
    })
  )
  windows$hue <- .chip_hue(windows$chip)
  rc <- .page_coords(ref$L, ref$a, ref$b)
  windows$hex <- rc$hex[match(windows$chip, ref$chip)]

  pc <- .page_coords(res$L, res$a, res$b)
  pts <- data.frame(
    book_id = res$book_id,
    chip = res$chip,
    hue = res$hue,
    x = pc$x,
    y = pc$y,
    hex = pc$hex,
    verdict = ifelse(res$fail, "Outside", "Inside"),
    stringsAsFactors = FALSE,
  )

  pts$draw <- ifelse(res$fail, pts$hex, "grey35")

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = windows,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = interaction(.data$chip, .data$cut),
        fill = .data$hex
      ),
      alpha = 0.18,
      color = NA,
    ) +
    ggplot2::geom_path(
      data = windows,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = interaction(.data$chip, .data$cut),
        color = .data$hex,
        linetype = .data$cut
      ),
      linewidth = 0.4
    ) +
    ggplot2::geom_point(
      data = pts,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        color = .data$draw,
        shape = .data$verdict
      ),
      size = 2.4,
      alpha = 0.85
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_shape_manual(values = c(Inside = 1, Outside = 16)) +
    ggplot2::labs(
      x = expression("Chroma (" * C^"*" / 6.72 * ")"),
      y = expression("Value  (" * L^"*" / 10 * ")"),
      shape = "In tolerance?",
      linetype = expression(Delta * E["00"] * " window"),
      title = "Chips in Munsell page space"
    ) +
    ggplot2::coord_fixed(ratio = 1.5) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2),
      legend.position = "right"
    )

  multi_book <- length(unique(pts$book_id)) > 1L
  if (facet_books && multi_book) {
    p + ggplot2::facet_grid(book_id ~ hue)
  } else {
    p + ggplot2::facet_wrap(~hue)
  }
}
