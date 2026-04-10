#' @keywords internal
"_PACKAGE"

#' Create a munqc scan data table
#' 
#' The primary way to create a validated scan data object from vectors or pre-loaded dataframe.
#' Data can be provided using one of two schemes:
#' * **condensed munsell:** columns `Chip`, `L*`, `a*`, `b*`
#' * **expanded munsell:** columns `Hue`, `Value`, `Chroma`, `L*`, `a*`, `b*`
#' 
#' @param data A data frame containing at Lab coordinates and at least one Munsell chip identifier scheme. May be the result of [base::read.csv()], [read_scan_file()], or [data.frame()].
#' @param sensor Character string naming the sensor used. Currently, supported sensors are (`"veykolor"`, `"nix"`, `"colormuse"`, `"konicaminolta"`). Matching a sensor used in the reference dataset is important for accuracy.
#' @param chip_col Name of the chip ID column when usig chip-only scheme. Defaults to `"chip"`.
#' @param L_col,a_col,b_col Column names for L\*, a\*, and b\* values. Defaults to `"L"`, `"a"`, `"b"`.
#' @param hue_col,value_col,chroma_col Column names for Munsell hue, value, and chroma when using expanded munsell scheme. Defaults to `"h"`, `"v"`, `"c"`.
#' 
new_scan_collection <- function(
  data,
  sensor,
  chip_col = "chip",
  L_col = "L",
  a_col = "a",
  b_col = "b",
  hue_col = "h",
  value_col = "v",
  chroma_col = "c"
) {
  data <- as.data.frame(data)
  nms <- names(data)

  has_chip <- chip_col %in% nms
  has_munsell <- all(c(hue_col, value_col, chroma_col) %in% nms)

  if (!has_chip && !has_munsell) {
    cli::cli_abort(c(
      "Cannot identify chips in {.arg data}.",
      "i" = "Supply a {.field {chip_col}} column, or all of \\
        {.field {hue_col}}, {.field {value_col}}, or {.field {chroma_col}}."
    ))
  }

  lab_cols <- c(L_col, a_col, b_col)
  missing_lab <- setdiff(lab_cols, nms)
  if (length(missing_lab) > 0) {
    cli::cli_abort
  }
}

read_scan_file <- function(path = "data.csv", sensor = "veykolor") {
  df <- read.csv(path)
  new_scan_collection(df, sensor = sensor)
}
