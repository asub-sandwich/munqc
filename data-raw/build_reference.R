library(dplyr)
library(tidyr)
library(readr)

SCAN_FILES <- list(
  colormuse = "data-raw/ref_scans/colormuse.csv",
  nix = "data-raw/ref_scans/nix.csv",
  veykolor = "data-raw/ref_scans/veykolor.csv"
)

FINISH_FILE <- list(finish = "data-raw/ref_scans/finish.csv")

# -- Read and bind all scanning data

required_cols <- c("chip", "L", "a", "b")

raw_list <- lapply(names(SCAN_FILES), function(sensor) {
  path <- SCAN_FILES[[sensor]]
  if (!file.exists(path)) {
    warning("File not found, skipping: ", path)
    return(NULL)
  }
  df <- read.csv(path)
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Sensor '%s': missing columns: %s",
      sensor,
      paste(missing, collapse = ", ")
    ))
  }

  df$sensor <- sensor
  df[, c(
    "sensor",
    required_cols,
    setdiff(names(df), c("sensor", required_cols))
  )]
})

raw_all <- dplyr::bind_rows(Filter(Negate(is.null), raw_list))

# -- Summary statistics of scan data
ref_stats <- raw_all |>
  dplyr::group_by(chip, sensor) |>
  dplyr::summarise(
    n_reps = dplyr::n(),
    L_mean = mean(L, na.rm = TRUE),
    a_mean = mean(a, na.rm = TRUE),
    b_mean = mean(b, na.rm = TRUE),
    L_sd = sd(L, na.rm = TRUE),
    a_sd = sd(a, na.rm = TRUE),
    b_sd = sd(b, na.rm = TRUE)
  ) |>
  ungroup() |>
  dplyr::mutate(
    L_sd = dplyr::if_else(is.na(L_sd), 0, L_sd),
    a_sd = dplyr::if_else(is.na(a_sd), 0, a_sd),
    b_sd = dplyr::if_else(is.na(b_sd), 0, b_sd),
  )
