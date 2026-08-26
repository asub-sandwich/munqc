#' Normalize a column name for fuzzy matching
#' @noRd
.norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

#' Candidate column names for each role
#' @noRd
COLUMN_CANDIDATES <- list(
  chip = c(
    "chip",
    "chipid",
    "chipname",
    "munsell",
    "munsellnotation",
    "notation",
    "colorchip",
    "colourchip",
    "color",
    "colour"
  ),
  L = c("l", "lstar", "ciel", "cielabl", "lightness", "lvalue"),
  a = c("a", "astar", "ciea", "cielaba", "avalue"),
  b = c("b", "bstar", "cieb", "cielabb", "bvalue"),
  hue = c("hue", "h", "munsellhue"),
  value = c("value", "v", "munsellvalue"),
  chroma = c("chroma", "c", "munsellchroma"),
  book_id = c("bookid", "book", "booknumber", "bookno", "sampleid", "sample"),
  finish = c("finish", "gloss", "glossiness", "sheen", "surface")
)

#' Resolve one role to a column in `nms`
#'
#' Returns the column name, or NA if nothing matched. Errors when the guess is
#' ambiguous, because silently picking one of two plausible columns is worse
#' than making the user say which they meant.
#' @noRd
.match_column <- function(role, nms, explicit = NULL) {
  if (!is.null(explicit)) {
    if (!explicit %in% nms) {
      stop(
        sprintf(
          "Column \"%s\" (given for `%s`) is not in the file. Available: %s.",
          explicit,
          role,
          paste(nms, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(explicit)
  }

  hits <- nms[.norm_name(nms) %in% COLUMN_CANDIDATES[[role]]]
  if (length(hits) == 0L) {
    return(NA_character_)
  }
  if (length(hits) > 1L) {
    stop(
      sprintf(
        "Ambiguous columns for `%s`: %s. Pass %s = \"...\" to choose.",
        role,
        paste(hits, collapse = ", "),
        role
      ),
      call. = FALSE
    )
  }
  hits
}

#' Join separate hue/value/chroma columns into condensed munsell notation.
#' @noRd
.condense_munsell <- function(hue, value, chroma) {
  paste0(trimws(hue), " ", trimws(value), "/", trimws(chroma))
}

#' Read one tabular file into a plain data.frame
#' @noRd
.read_table <- function(path, sheet = NULL, ...) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Reading Excel files needs the \"readxl\" package. ",
        "Install it with install.packages(\"readxl\"), or export to CSV.",
        call. = FALSE
      )
    }
    as.data.frame(
      readxl::read_excel(path, sheet = sheet %||% 1L, ...),
      stringsAsFactors = FALSE
    )
  } else if (ext %in% c("csv", "txt")) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
  } else if (ext %in% c("tsv", "tab")) {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
  } else {
    stop(
      sprintf(
        "Don't know how to read \".%s\". Supported: csv, tsv, txt, xlsx, xls.",
        ext
      ),
      call. = FALSE
    )
  }
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Read colour scans from CSV or Excel
#'
#' Reads one or more scan files and returns a [scan_collection()] ready for
#' [compute_error()].
#'
#' Column names are detected automatically where possible: `chip`, `L`, `a`,
#' `b` and common variants (`L*`, `lightness`, `munsell`, `book`, ...) are
#' recognised case-insensitively and ignoring punctuation. Anything the
#' detector cannot find, or finds twice, must be named explicitly.
#'
#' Chips may be given either as condensed notation in a single column
#' (`"10YR 8/1"`) or as separate `hue`, `value` and `chroma` columns, which are
#' joined for you.
#'
#' @param path Path to a `.csv`, `.tsv`, `.txt`, `.xlsx` or `.xls` file. May be
#'   a vector of paths, in which case each file becomes one book.
#' @param sensor,illuminant,observer Passed to [scan_collection()].
#' @param chip,L,a,b Names of the corresponding columns. `NULL` to autodetect.
#' @param hue,value,chroma Names of separate Munsell component columns, used
#'   when there is no single condensed `chip` column.
#' @param book_id Either the name of a column holding book identifiers, or a
#'   literal label to apply to every row. When `NULL`, the file name is used.
#' @param finish Name of a column giving each chip's surface finish. Usually
#'   unnecessary, since finishes are looked up from [chipfinish].
#' @param sheet Sheet name or number, for Excel files only.
#' @param ... Passed on to [utils::read.csv()] or [readxl::read_excel()].
#'
#' @return A `ScanCollection`.
#'
#' @examples
#' f <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(
#'     Munsell = c("10YR 8/1", "10YR 8/2"),
#'     `L*` = c(81.3, 80.9), `a*` = c(1.3, 3.1), `b*` = c(6.6, 13.5),
#'     check.names = FALSE
#'   ),
#'   f, row.names = FALSE
#' )
#'
#' read_scan(f, sensor = "veykolor")
#'
#' # Explicit mapping when autodetection can't help
#' read_scan(f, sensor = "veykolor", chip = "Munsell", L = "L*")
#'
#' unlink(f)
#'
#' @export
read_scan <- function(
  path,
  sensor = SENSORS,
  illuminant = ILLUMINANTS,
  observer = OBSERVERS,
  chip = NULL,
  L = NULL,
  a = NULL,
  b = NULL,
  hue = NULL,
  value = NULL,
  chroma = NULL,
  book_id = NULL,
  finish = NULL,
  sheet = NULL,
  ...
) {
  if (!is.character(path) || length(path) == 0L) {
    stop("`path` must be one or more file paths.", call. = FALSE)
  }

  parts <- lapply(path, function(p) {
    df <- .read_table(p, sheet = sheet, ...)
    if (nrow(df) == 0L) {
      stop(sprintf("No rows in %s.", p), call. = FALSE)
    }
    .tidy_scan(
      df,
      chip = chip,
      L = L,
      a = a,
      b = b,
      hue = hue,
      value = value,
      chroma = chroma,
      book_id = book_id,
      finish = finish,
      default_book = tools::file_path_sans_ext(basename(p)),
      source = p
    )
  })

  scan_collection(
    do.call(rbind, parts),
    sensor = sensor,
    illuminant = illuminant,
    observer = observer
  )
}

#' Map an arbitrary data.frame onto munqc's expected columns
#' @noRd
.tidy_scan <- function(
  df,
  chip,
  L,
  a,
  b,
  hue,
  value,
  chroma,
  book_id,
  finish,
  default_book,
  source
) {
  nms <- names(df)

  col_L <- .match_column("L", nms, L)
  col_a <- .match_column("a", nms, a)
  col_b <- .match_column("b", nms, b)

  missing <- c("L", "a", "b")[is.na(c(col_L, col_a, col_b))]
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Could not find Lab column%s %s in %s.\n  Columns present: %s\n  Name them explicitly, e.g. read_scan(path, %s = \"...\").",
        if (length(missing) > 1L) "s" else "",
        paste(missing, collapse = ", "),
        source,
        paste(nms, collapse = ", "),
        missing[1]
      ),
      call. = FALSE
    )
  }

  col_chip <- .match_column("chip", nms, chip)
  if (!is.na(col_chip)) {
    chip_vec <- as.character(df[[col_chip]])
  } else {
    col_h <- .match_column("hue", nms, hue)
    col_v <- .match_column("value", nms, value)
    col_c <- .match_column("chroma", nms, chroma)
    if (anyNA(c(col_h, col_v, col_c))) {
      stop(
        sprintf(
          "Could not identify chips in %s.\n  Columns present: %s\n  Supply either a condensed notation column (chip = \"...\") or all three of hue, value and chroma.",
          source,
          paste(nms, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    chip_vec <- .condense_munsell(df[[col_h]], df[[col_v]], df[[col_c]])
  }

  # book_id names a column if it matches one, otherwise it is a literal label.
  if (is.null(book_id)) {
    col_book <- .match_column("book_id", nms, NULL)
    book_vec <- if (is.na(col_book)) {
      default_book
    } else {
      as.character(df[[col_book]])
    }
  } else if (book_id %in% nms) {
    book_vec <- as.character(df[[book_id]])
  } else {
    book_vec <- book_id
  }

  out <- data.frame(
    book_id = book_vec,
    chip = chip_vec,
    L = suppressWarnings(as.numeric(df[[col_L]])),
    a = suppressWarnings(as.numeric(df[[col_a]])),
    b = suppressWarnings(as.numeric(df[[col_b]])),
    stringsAsFactors = FALSE
  )

  col_finish <- .match_column("finish", nms, finish)
  if (!is.na(col_finish)) {
    out$finish <- .normalize_finish(df[[col_finish]])
  }

  bad <- is.na(out$L) | is.na(out$a) | is.na(out$b)
  if (any(bad)) {
    warning(
      sprintf(
        "Dropped %d row%s from %s with unreadable Lab values.",
        sum(bad),
        if (sum(bad) > 1L) "s" else "",
        source
      ),
      call. = FALSE
    )
    out <- out[!bad, , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}
