#' Munsell notation regex matching
#' @noRd
MUNSELL_RE <- "^\\s*([0-9]+(?:\\.[0-9]+)?[A-Za-z]+)\\s+([0-9]+(?:\\.[0-9]+)?)/([0-9]+(?:\\.[0-9]+)?)\\s*$"

#' Validate and trim munsell notation
#'
#' Returns `NA` for anything that does not parse.
#' @noRd
.normalize_chip <- function(chip) {
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

utils::globalVariables(c("colordata", "colordata_raw", "metadata", "book_id"))
