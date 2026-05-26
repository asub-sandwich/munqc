#' condense individual munsell components into a single character string
#' @noRd
.condense_munsell <- function(h, v, c) {
  paste0(h, " ", v, "/", c)
}

#' expand character string of munsell notation into its separate parts
#' @noRd
.expand_munsell <- function(hvc) {
  h_vc <- unlist(strsplit(hvc, " "))
  h <- h_vc[1]
  vc <- h_vc[2]
  v_c <- unlist(strsplit(vc, "/"))
  v <- v_c[1]
  c <- v_c[2]
  c(h, v, c)
}

#' get hue of condensed munsell notation
#' @noRd
.get_hue <- function(hvc) {
  .expand_munsell(hvc)[1]
}

#' get value of condensed munsell notation
#' @noRd
.get_value <- function(hvc) {
  .expand_munsell(hvc)[2]
}

#' get chroma of condensed munsell notation
#' @noRd
.get_chroma <- function(hvc) {
  .expand_munsell(hvc)[3]
}

#' validate glossiness character strings
#' @noRd
.validate_glossiness <- function(data) {
  gloss <- data$unq <- unique(gloss)
  if (length(unq) > 3) {
    cli::cli_abort(
      "{.field {gloss}} should only contain `gloss`, `semigloss`, and `matte`!"
    )
  }

  grepl()
}

#' munq scan collection
#'
#' Create a validated munq scan data object
#'
#' @param data [data.frame] or type that can be coerced to one. Contains at least munsell identifier and Lab coordinates.
#' @param sensor Character string naming the sensor used.
#' @param chip_col Name of the munsell chip ID column.
#' @param L_col,a_col,b_col Name of the column names for the CIE Lab coordinates.
#' @param h_col,v_col,c_col Name of the column names for the Munsell hue, value, and chroma.
#' @param gloss_col Name of the column containing glossiness data. (Optional)
#'
#' @return A `munq` S3 object
#'
#' @examples
#' # from condensed Munsell notation
#' df <- data.frame(
#'   chip = c("10YR 3/2", "7.5YR 4/4"),
#'   L = c(30.5, 41.9),
#'   a = c(5.1, 12.3),
#'   b = c(12.4, 23.6)
#' )
#'
#' scan <- munq_scan(df, sensor="veykolor")
#'
#' # from expanded Munsell notation
#' df2 <- data.frame(
#'   h = c("10YR", "7.5YR"),
#'   v = c(3, 4),
#'   c = c(2, 4),
#'   L = c(30.5, 41.9),
#'   a = c(5.1, 12.3),
#'   b = c(12.4, 23.6)
#' )
#' scan2 <- munq_scan(df2, sensor="veykolor")
#'
#' @export
munq_scan <- function(
  data,
  sensor = c("colormuse", "nix", "veykolor", "konicaminolta"),
  chip_col = "chip",
  L_col = "L",
  a_col = "a",
  b_col = "b",
  h_col = "h",
  v_col = "v",
  c_col = "c",
  gloss_col = "gloss"
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} is not coercable to data.frame!")
  }
  data <- as.data.frame(data)
  nms <- names(data)

  has_chip <- chip_col %in% nms
  has_munsell <- all(c(h_col, v_col, c_col) %in% nms)

  if (!has_chip && !has_munsell) {
    cli::cli_abort(c(
      "Cannot identify Munsell chips in {.arg data}.",
      "i" = "Supply a {.field {chip_col}} column or all of \\
        {.field {h_col}}, {.field {v_col}}, and {.field {c_col}}."
    ))
  }

  lab_cols <- c(L_col, a_col, b_col)
  missing_lab <- setdiff(lab_cols, nms)
  if (length(missing_lab) > 0) {
    cli::cli_abort("Missing required Lab column{?s}: {.field {missing_lab}}.")
  }

  if (has_chip) {
    chip_id <- as.character(data[[chip_col]])
    h <- as.character(sapply(data[[h_col]], .get_hue))
    v <- as.character(sapply(data[[v_col]], .get_value))
    c <- as.character(sapply(data[[c_col]], .get_chroma))
  } else {
    chip_id <- as.character(sapply(
      data[, c(h_col, v_col, c_col)],
      function(x) {
        .condense_munsell(x[h_col], x[v_col], x[c_col])
      }
    ))
    h <- as.character(data[[h_col]])
    v <- as.character(data[[v_col]])
    c <- as.character(data[[c_col]])
  }

  df <- data.frame(
    chip_id = chip_id,
    hue = h,
    value = c,
    L = as.numeric(data[[L.col]]),
    a = as.numeric(data[[a.col]]),
    b = as.numeric(data[[b.col]]),
  )

  df$gloss <- .validate_glossiness(df)
}
