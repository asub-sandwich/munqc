.condense_munsell <- function(h, v, c) {
  paste0(h, " ", v, "/", c)
}

.expand_munsell <- function(hvc) {
  h_vc <- unlist(strsplit(hvc, " "))
  h <- h_vc[1]
  vc <- h_vc[2]
  v_c <- unlist(strsplit(vc, "/"))
  v <- v_c[1]
  c <- v_c[2]
  c(h, v, c)
}

.get_hue <- function(hvc) {
  .expand_munsell(hvc)[1]
}

.get_value <- function(hvc) {
  .expand_munsell(hvc)[2]
}

.get_chroma <- function(hvc) {
  .expand_munsell(hvc)[3]
}

#' munq scan collection
#'
#' The primary way to create a validated scan data object from vectors or pre-loaded dataframe.
#' Data can be provided using one of two schemes:
#' * **condensed munsell:** columns `Chip`, `L`, `a`, `b`
#' * **expanded munsell:** columns `Hue`, `Value`, `Chroma`, `L`, `a`, `b`
#'
#' @param data A data frame containing at Lab coordinates and at least one Munsell chip identifier scheme. May be the result of `read.csv()`, `read_scan_file()`, or `data.frame()`.
#' @param sensor Character string naming the sensor used. Currently, supported sensors are (`"veykolor"`, `"nix"`, `"colormuse"`, `"konicaminolta"`). Matching a sensor used in the reference dataset is important for accuracy.
#' @param chip_col Name of the chip ID column when usig chip-only scheme. Defaults to `"chip"`.
#' @param L_col,a_col,b_col Column names for L, a, and b values. Defaults to `"L"`, `"a"`, `"b"`.
#' @param hue_col,value_col,chroma_col Column names for Munsell hue, value, and chroma when using expanded munsell scheme_ Defaults to `"h"`, `"v"`, `"c"`_
#' @param gloss_col Character string that describes the finish of the scanned chip, one of (`"matte"`, `"semiglossy"`, `"glossy"`). Defaults to `matte`. Only used to ensure that the glossiness of scanned chips is comparable to the reference dataset.
#'
#' @return A `munq` S3 object with:
#'   * `data` - scan data
#'     * `chip.id` - character chip identifier
#'     * `hue`, `value`, `chroma` - Munsell Notation
#'     * `L`, `a`, `b` - CIE Lab coordinates. Not to be confused with Hunter Lab coordinates.
#'     * `gloss` - The supplied glossiness of the chip
#'   * `sensor` - The supplied sensor identifier
#'   *
#'
#' @examples
#' # From condensed Munsell notation
#' df <- data.frame(
#'   chip = c("10YR 3/2", "7.5YR 4/4"),
#'   L = c(30.5, 41.9),
#'   a = c(5.1, 12.3),
#'   b = c(12.4, 23.6)
#' )
#' scan <- munq.scan(df, sensor="veykolor")
#'
#' # From expanded Munsell notation
#' df2 <- data.frame(
#'   h = c("10YR", "7.5YR"),
#'   v = c(3, 4),
#'   c = c(2, 4),
#'   L = c(30.5, 41.9),
#'   a = c(5.1, 12.3),
#'   b = c(12.4, 23.6)
#' )
#' scan2 <- munq.scan(df2, sensor="veykolor")
#'
#' @export
munq.scan <- function(
  data,
  sensor,
  chip.col = "chip",
  L.col = "L",
  a.col = "a",
  b.col = "b",
  hue.col = "h",
  value.col = "v",
  chroma.col = "c",
  gloss.col = "gloss"
) {
  data <- as.data.frame(data)
  nms <- names(data)

  has.chip <- chip.col %in% nms
  has.munsell <- all(c(hue.col, value.col, chroma.col) %in% nms)

  if (!has.chip && !has.munsell) {
    cli::cli_abort(c(
      "Cannot identify chips in {.arg data}.",
      "i" = "Supply a {.field {chip.col}} column, or all of \\
        {.field {hue.col}}, {.field {value.col}}, or {.field {chroma.col}}."
    ))
  }

  lab.cols <- c(L.col, a.col, b.col)
  missing.lab <- setdiff(lab.cols, nms)
  if (length(missing.lab) > 0) {
    cli::cli_abort(
      "Missing required Lab column{?s}: {.field {missing.lab}}."
    )
  }

  if (has.chip) {
    chip.id <- as.character(data[[chip.col]])
    h <- as.character(sapply(data[[chip.col]], .get.hue))
    v <- as.numeric(sapply(data[[value.col]], .get.value))
    c <- as.numeric(sapply(data[[chroma.col]], .get.chroma))
  } else {
    chip.id <- as.character(sapply(
      data[, c(hue.col, value.col, chroma.col)],
      function(x) {
        .condense.munsell(x[hue.col], x[value.col], x[chroma.col])
      }
    ))
    h <- as.character(data[[hue.col]])
    v <- as.numeric(data[[value.col]])
    c <- as.numeric(data[[chroma.col]])
  }

  out <- data.frame(
    chip.id = chip.id,
    hue = h,
    value = v,
    chroma = c,
    L = as.numeric(data[[L.col]]),
    a = as.numeric(data[[a.col]]),
    b = as.numeric(data[[b.col]]),
    sensor = sensor,
    stringsAsFactors = FALSE
  )
  validate.scan(out)
  structure(out, class = c("munqc.collection", "data.frame"))
}

#' @rdname munq.scan
#' @param ... Named vectors for each column, passed directly. Column names must
#'   follow the same conventions as `new.scan()`. A `sensor` argument is
#'   required.
#' @export
scan_from_vectors <- function(
  sensor,
  chip = NULL,
  h = NULL,
  v = NULL,
  c = NULL,
  L,
  a,
  b
) {
  if (!is.null(chip)) {
    df <- data.frame(chip = chip, L = L, a = a, b = b, stringsAsFactors = FALSE)
  } else if (!is.null(h)) {
    df <- data.frame(
      h = h,
      v = v,
      c = c,
      L = L,
      a = a,
      b = b,
      stringsAsFactors = FALSE
    )
  } else {
    cli::cli_abort(
      "Supply either {.arg chip} or all of {.arg h}, {.arg v}, {.arg c}."
    )
  }

  munq.scan(df, sensor = sensor)
}

#' Read a scan file (CSV or TXT) into a munqc scan collection tibble.
#'
#' Reads a delimited file and auto-detects which chip identification scheme is
#' present. Recognised column-name patterns are case-insensitive and accept
#' common variants (e.g. `"L*"`, `"Lstar"`, `"L_star"` all map to `L`).
#'
#' **Supported layouts**
#' | Layout | Required columns |
#' |--------|-----------------|
#' | chip-only | `chip`, `L`, `a`, `b` |
#' | full Munsell | `h`, `v`, `c`, `L`, `a`, `b` |
#'
#' @param path Path to a `.csv` or `.txt` file.
#' @param sensor Character string naming the sensor. Required.
#' @param delim Delimiter character. `NULL` (default) auto-detects from the
#'   file extension: `,` for `.csv`, `\t` for `.txt`.
#' @param ... Additional arguments passed to [read.table()].
#'
#' @return A `munq.scan` object (see [munq.scan()]).
#'
#' @examples
#' \dontrun{
#' scan <- read.scan.file("my_scan.csv", sensor = "nix")
#' scan <- read.scan.file("my_scan.txt", sensor = "konicaminolta", delim = ",")
#' }
#'
#' @export
read.scan.file <- function(path, sensor, delim = NULL, ...) {
  path <- normalizePath(path, mustWork = TRUE)
  ext <- tolower(tools::file_ext(path))

  if (is.null(delim)) {
    delim <- if (ext == "csv") "," else "\t"
  }

  raw <- utils::read.table(
    path,
    header = TRUE,
    sep = delim,
    strip.white = TRUE,
    stringsAsFactors = FALSE,
    ...
  )

  raw <- .normalize_colnames(raw)
  munq.scan(raw, sensor = sensor)
}

#' Normalize column names to the expected scheme
#' @noRd
.normalize_colnames <- function(df) {
  nms <- names(df)

  # normalize L's
  nms <- gsub("^l[_\\s]?\\*?$", "L", nms, ignore.case = TRUE, perl = TRUE)
  nms <- gsub("^lstar$", "L", nms, ignore.case = TRUE)
  # normalize a's
  nms <- gsub("^a[_\\s]?\\*?$", "a", nms, ignore.case = TRUE, perl = TRUE)
  nms <- gsub("^astar$", "a", nms, ignore.case = TRUE)
  # normalize b's
  nms <- gsub("^b[_\\s]?\\*?$", "b", nms, ignore.case = TRUE, perl = TRUE)
  nms <- gsub("^bstar$", "b", nms, ignore.case = TRUE)
  # normalize munsell components
  nms <- gsub("^hue$", "h", nms, ignore.case = TRUE)
  nms <- gsub("^(munsell_?)?value$", "v", nms, ignore.case = TRUE, perl = TRUE)
  nms <- gsub("^chroma$", "c", nms, ignore.case = TRUE)
  # normalize chip id
  nms <- gsub(
    "^(chip[_-]?id|chip_no)$",
    "chip",
    nms,
    ignore.case = TRUE,
    perl = TRUE
  )

  names(df) <- nms
  df
}

#' validate munq.scan data.frame
#' @noRd
validate.scan <- function(df) {
  if (any(is.na(df$chip.id) | df$chip.id == "")) {
    cli::cli_abort("Some rows have missing or empty {.field {chip.id}} values.")
  }

  for (col in c("L", "a", "b")) {
    if (any(is.na(df[[col]]))) {
      cli::cli_warn(
        "Column {.field {col}} contains {sum(is.na(df[[col]]))} NA value(s)."
      )
    }
  }

  bad.L <- !is.na(df$L) & (df$L < 0 | df$L > 100)
  if (any(bad.L)) {
    cli::cli_warn(
      "{sum(bad.L)} row{?s} have L* values outside of [0, 100]. \\
      Check unit conversions and/or raw data for errors."
    )
  }

  invisible(df)
}

#' @export
print.munq.scan <- function(x, ...) {
  n.chips <- nrow(x)
  sensor <- unique(x$sensor)

  cli::cli_inform(c(
    "v" = "munq scan   [{n.chips} chip{?s}]",
    "*" = "Sensor:    {sensor}",
    "*" = "L* range:  [{round(min(x$L, na.rm=TRUE),1)}, {round(max(x$L, na.rm=TRUE),1)}]"
  ))
  NextMethod()
}
