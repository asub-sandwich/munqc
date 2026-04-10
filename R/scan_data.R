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
