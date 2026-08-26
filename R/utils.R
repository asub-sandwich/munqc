#' Regex matching condensed Munsell notation, e.g. "10YR 8/1", "2.5Y 6/4"
#' @noRd
MUNSELL_RE <- "^\\s*([0-9]+(?:\\.[0-9]+)?[A-Za-z]+)\\s+([0-9]+(?:\\.[0-9]+)?)/([0-9]+(?:\\.[0-9]+)?)\\s*$"

#' Trim and validate condensed Munsell notation
#'
#' Returns `NA` for anything that does not parse, so callers can decide whether
#' to warn or stop.
#' @noRd
.normalise_chip <- function(chip) {
  chip <- trimws(as.character(chip))
  m <- regmatches(chip, regexec(MUNSELL_RE, chip))
  vapply(
    m,
    function(p) {
      if (length(p) == 4L) paste0(p[2], " ", p[3], "/", p[4]) else NA_character_
    },
    character(1)
  )
}

#' @noRd
.chip_hue <- function(chip) {
  sub(MUNSELL_RE, "\\1", trimws(as.character(chip)))
}

#' @noRd
.chip_value <- function(chip) {
  as.numeric(sub(MUNSELL_RE, "\\2", trimws(as.character(chip))))
}

#' @noRd
.chip_chroma <- function(chip) {
  as.numeric(sub(MUNSELL_RE, "\\3", trimws(as.character(chip))))
}

# Columns referenced by non-standard evaluation, declared so R CMD check does
# not report them as undefined globals.
utils::globalVariables(c(
  "colordata",
  "colordata_raw",
  "metadata",
  "chipfinish",
  "book_id"
))

#' Resolve each chip's surface finish
#'
#' Prefers a finish the user supplied with their own scans, falling back to the
#' shipped [chipfinish] lookup. Chips in neither get `NA`, which downstream
#' code treats as matte (and therefore decisive).
#' @noRd
.chip_finish <- function(chip, user_finish = NULL) {
  out <- rep(NA_character_, length(chip))
  if (!is.null(user_finish)) {
    out <- .normalize_finish(user_finish)
  }
  need <- is.na(out)
  if (any(need)) {
    out[need] <- chipfinish$finish[match(chip[need], chipfinish$chip)]
  }
  out
}
