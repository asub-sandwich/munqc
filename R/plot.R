#' Default fill colours for threshold bands
#'
#' Green through red, interpolated to however many bands the user defined.
#' @noRd
.band_palette <- function(n) {
  grDevices::colorRampPalette(
    c("#2C7A4B", "#7FB069", "#E8C547", "#E07A3F", "#B23A2E")
  )(n)
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
#' @param type One of `"histogram"` (distribution of \eqn{\Delta E_{00}} with
#'   threshold bands shaded behind it), `"ecdf"` (cumulative distribution, so
#'   you can read off what fraction of chips passes at *any* threshold, not
#'   just the one you chose), `"page"` (one row per Munsell hue page, so a
#'   single bad page stands out), or `"finish"` (one row per surface finish,
#'   which shows at a glance whether a book's problems are confined to its
#'   glossy chips).
#' @param binwidth Histogram bin width. Defaults to `0.25`.
#' @param facet_books Facet by `book_id` when more than one book is present.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' sc <- compute_error(scan_collection(my_scans, sensor = "nix"))
#' plot(sc)
#' plot(sc, type = "ecdf")
#' plot(sc, type = "page")
#' }
#'
#' @export
plot.ScanCollection <- function(
  x,
  type = c("histogram", "ecdf", "page", "finish"),
  binwidth = 0.25,
  facet_books = TRUE,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" is required for plotting. Install it with ",
      "install.packages(\"ggplot2\").",
      call. = FALSE
    )
  }
  if (!is_scan_collection(x)) {
    stop("`x` must be a ScanCollection.", call. = FALSE)
  }
  if (is.null(x$results)) {
    stop("No results yet. Call compute_error() first.", call. = FALSE)
  }
  type <- match.arg(type)

  res <- x$results
  th <- x$thresholds
  bands <- .band_frame(th, upper = max(res$delta_e_2000, na.rm = TRUE))
  pal <- stats::setNames(.band_palette(length(th$labels)), th$labels)
  multi_book <- length(unique(res$book_id)) > 1L

  # One dashed line per distinct fail cut, so a per-finish policy is visible.
  fail_line <- ggplot2::geom_vline(
    xintercept = unique(th$fail_at),
    linetype = "dashed",
    linewidth = 0.6,
    colour = "grey20"
  )

  p <- switch(
    type,
    histogram = ggplot2::ggplot(res) +
      ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$grade),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.18
      ) +
      ggplot2::geom_histogram(
        ggplot2::aes(x = .data$delta_e_2000),
        binwidth = binwidth,
        boundary = 0,
        fill = "grey25",
        colour = "white",
        linewidth = 0.2
      ) +
      fail_line +
      ggplot2::labs(
        x = expression(Delta * E[00]),
        y = "Chips",
        title = "Colour difference from reference",
        subtitle = sprintf(
          "%s  |  %d of %d chips failing (%d decisive, %d advisory)",
          x$sensor,
          sum(res$fail),
          nrow(res),
          sum(res$fail & res$decisive),
          sum(res$fail & !res$decisive)
        )
      ),

    ecdf = ggplot2::ggplot(res) +
      ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$grade),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.18
      ) +
      ggplot2::stat_ecdf(
        ggplot2::aes(x = .data$delta_e_2000),
        geom = "step",
        linewidth = 0.8,
        colour = "grey15"
      ) +
      fail_line +
      ggplot2::scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
      ggplot2::labs(
        x = expression(Delta * E[00]),
        y = "Chips at or below",
        title = "Cumulative colour difference",
        subtitle = "Read off the pass rate at any threshold, not just the chosen one"
      ),

    page = ggplot2::ggplot(res) +
      ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$grade),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.18
      ) +
      ggplot2::geom_boxplot(
        ggplot2::aes(x = .data$delta_e_2000, y = .data$hue),
        outlier.size = 1,
        width = 0.55,
        fill = "white",
        colour = "grey15",
        linewidth = 0.4
      ) +
      fail_line +
      ggplot2::labs(
        x = expression(Delta * E[00]),
        y = "Page (Munsell hue)",
        title = "Colour difference by page"
      ),

    finish = ggplot2::ggplot(res) +
      ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$grade),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.18
      ) +
      ggplot2::geom_boxplot(
        ggplot2::aes(
          x = .data$delta_e_2000,
          y = factor(.data$finish, levels = FINISHES)
        ),
        outlier.size = 1,
        width = 0.55,
        fill = "white",
        colour = "grey15",
        linewidth = 0.4
      ) +
      ggplot2::geom_jitter(
        ggplot2::aes(
          x = .data$delta_e_2000,
          y = factor(.data$finish, levels = FINISHES)
        ),
        height = 0.12,
        size = 0.7,
        alpha = 0.4
      ) +
      fail_line +
      ggplot2::labs(
        x = expression(Delta * E[00]),
        y = "Surface finish",
        title = "Colour difference by chip finish",
        subtitle = sprintf(
          "Verdict decided by: %s",
          paste(th$decisive, collapse = ", ")
        )
      )
  )

  p <- p +
    ggplot2::scale_fill_manual(values = pal, name = NULL, drop = FALSE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  if (facet_books && multi_book) {
    p <- p + ggplot2::facet_wrap(~book_id)
  }
  p
}
