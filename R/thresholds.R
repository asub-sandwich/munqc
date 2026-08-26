#' Recognised chip surface finishes, glossiest last
#' @noRd
FINISHES <- c("matte", "semigloss", "gloss")

#' Coerce arbitrary finish labels to the canonical set
#' @noRd
.normalize_finish <- function(x) {
  x <- gsub("[^a-z]", "", tolower(trimws(as.character(x))))
  out <- rep(NA_character_, length(x))
  out[x %in% c("matte", "matt", "flat", "dull")] <- "matte"
  out[x %in% c("semigloss", "semi", "satin", "eggshell")] <- "semigloss"
  out[x %in% c("gloss", "glossy", "shiny")] <- "gloss"
  out
}

#' Quality-control thresholds and policy
#'
#' Defines the CIEDE2000 (\eqn{\Delta E_{00}}) cut points used to grade colour
#' chips, and which chips are allowed to condemn a whole book. A single
#' `munq_thresholds` object is shared by [compute_error()], [qc_summary()] and
#' the plotting methods.
#'
#' Because color is subjective, and there is no single value that determines
#' good versus bad when judging color books, there are three separate ideas
#' to help the user determine for themselves what is "good" and "bad".
#'
#' * `breaks` are *descriptive* bands, used for grading and for shading plots.
#'   They are always global, so every plot shares one colour scheme.
#' * `fail_at` is the *decision* cut for an individual chip. A chip fails when
#'   its \eqn{\Delta E_{00}} is at or above the cut. This may vary by finish.
#' * `decisive` names the finishes that count towards a book's verdict.
#'.
#'
#' @param breaks Increasing, strictly positive numeric vector of band edges.
#' @param labels Character vector naming each band. Must be one longer than
#'   `breaks`.
#' @param fail_at Either a single number applied to every finish, or a named
#'   numeric vector giving a per-finish cut, e.g.
#'   `c(matte = 3, semigloss = 4, gloss = 5)`. Unnamed finishes inherit the
#'   `matte` value. Every value must be one of `breaks`, so the pass/fail line
#'   always coincides with a band edge.
#' @param decisive Character vector of finishes that count towards a book or
#'   page verdict. Defaults to `"matte"`.
#'
#' @return An object of class `munq_thresholds`.
#'
#' @examples
#' munq_thresholds()
#'
#' # Loosen the per-chip cut on glossy chips as well as excluding them
#' # from the verdict
#' munq_thresholds(fail_at = c(matte = 3, semigloss = 3, gloss = 5))
#'
#' # Let semigloss count towards the verdict too
#' munq_thresholds(decisive = c("matte", "semigloss"))
#'
#' # Stricter throughout
#' munq_thresholds(fail_at = 1)
#'
#' @export
munq_thresholds <- function(
  breaks = c(1, 2, 3, 5),
  labels = c(
    "imperceptible",
    "perceptible",
    "acceptable",
    "marginal",
    "replace"
  ),
  fail_at = 3,
  decisive = "matte"
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

  if (!is.numeric(fail_at) || anyNA(fail_at) || length(fail_at) == 0L) {
    stop("`fail_at` must be numeric with no missing values.", call. = FALSE)
  }
  if (is.null(names(fail_at))) {
    if (length(fail_at) != 1L) {
      stop(
        "Unnamed `fail_at` must be a single number. To vary it by finish, ",
        "name the elements, e.g. c(matte = 3, gloss = 5).",
        call. = FALSE
      )
    }
    fail_at <- stats::setNames(rep(fail_at, length(FINISHES)), FINISHES)
  } else {
    unknown <- setdiff(names(fail_at), FINISHES)
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "Unknown finish%s in `fail_at`: %s. Known finishes: %s.",
          if (length(unknown) > 1L) "es" else "",
          paste(unknown, collapse = ", "),
          paste(FINISHES, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (!"matte" %in% names(fail_at)) {
      stop(
        "A named `fail_at` must include `matte`, which unnamed finishes inherit.",
        call. = FALSE
      )
    }
    full <- stats::setNames(rep(fail_at[["matte"]], length(FINISHES)), FINISHES)
    full[names(fail_at)] <- fail_at
    fail_at <- full
  }

  off_band <- fail_at[
    !vapply(
      fail_at,
      function(v) any(abs(breaks - v) < .Machine$double.eps^0.5),
      logical(1)
    )
  ]
  if (length(off_band) > 0L) {
    stop(
      sprintf(
        "`fail_at` value%s %s not among `breaks` (%s).",
        if (length(off_band) > 1L) "s" else "",
        paste(format(off_band), collapse = ", "),
        paste(format(breaks), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  decisive <- as.character(decisive)
  unknown <- setdiff(decisive, FINISHES)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown finish%s in `decisive`: %s.",
        if (length(unknown) > 1L) "es" else "",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(decisive) == 0L) {
    stop("`decisive` must name at least one finish.", call. = FALSE)
  }

  structure(
    list(
      breaks = as.numeric(breaks),
      labels = as.character(labels),
      fail_at = fail_at,
      decisive = decisive
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
  cat("<munq_thresholds>\n\nBands\n")
  cat(
    paste0(
      "  ",
      formatC(x$labels, width = -max(nchar(x$labels))),
      "  ",
      band,
      collapse = "\n"
    ),
    "\n"
  )

  cat("\nA chip fails at dE2000 >=\n")
  cat(
    paste0(
      "  ",
      formatC(names(x$fail_at), width = -max(nchar(names(x$fail_at)))),
      "  ",
      format(x$fail_at),
      ifelse(names(x$fail_at) %in% x$decisive, "", "   (advisory only)"),
      collapse = "\n"
    ),
    "\n"
  )

  cat(
    "\nBook verdict decided by: ",
    paste(x$decisive, collapse = ", "),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' Assign band labels to distances
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

#' Per-chip fail cut, given each chip's finish
#'
#' Chips with an unrecognised or missing finish fall back to the matte cut and
#' are treated as decisive, so an unlabelled chip is never quietly excused.
#' @noRd
.fail_cut <- function(finish, thresholds) {
  finish <- as.character(finish)
  finish[is.na(finish) | !finish %in% FINISHES] <- "matte"
  unname(thresholds$fail_at[finish])
}

#' Whether each chip counts towards a book verdict
#' @noRd
.is_decisive <- function(finish, thresholds) {
  finish <- as.character(finish)
  finish[is.na(finish) | !finish %in% FINISHES] <- "matte"
  finish %in% thresholds$decisive
}

#' Is a chip failing?
#'
#' Factored out so the boundary case is directly testable: a chip sitting
#' exactly on its cut fails. Bands are left-closed, so this keeps grading and
#' pass/fail agreeing at every edge.
#' @noRd
.is_fail <- function(delta_e, cut) {
  delta_e >= cut
}
