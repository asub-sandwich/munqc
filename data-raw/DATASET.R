### This file builds the reference datasets that are shipped with munqc.
### source("data-raw/DATASET.R")

library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)

raw_data_path <- "data-raw/newbook_data.xlsx"

col_names <- c(
  "chip10",
  "L10",
  "a10",
  "b10",
  "chip75",
  "L75",
  "a75",
  "b75",
  "chip25",
  "L25",
  "a25",
  "b25"
)

### Reorder native `wide` layout of colorchip and readings to `long` layout
format_data_sheet <- function(sheet) {
  names(sheet) <- col_names

  sheet %>%
    pivot_longer(
      cols = everything(),
      names_to = c(".value", "hue"),
      names_pattern = "^(chip|L|a|b)(\\d+)$"
    ) %>%
    select(-hue) %>%
    filter(!is.na(chip))
}

sensor_lookup <- c(
  VK = "veykolor",
  Nix = "nix",
  CM = "colormuse",
  CR400 = "konicaminolta"
)

data_sheets <- raw_data_path %>%
  excel_sheets() %>%
  str_subset("^NewBook_\\d+_")

stopifnot(length(data_sheets) == 12L)

colordata_raw <- data_sheets %>%
  set_names() %>%
  map(\(s) {
    read_excel(raw_data_path, sheet = s) %>%
      select(1:12) %>%
      format_data_sheet()
  }) %>%
  list_rbind(names_to = "sheet") %>%
  mutate(
    replicate = str_extract(sheet, "(?<=NewBook_)\\d+") %>% as.integer(),
    sensor = unname(sensor_lookup[str_extract(sheet, "[^_]+$")]),
    .before = 1
  ) %>%
  select(-sheet) %>%
  mutate(chip = str_squish(chip))

stopifnot(!anyNA(colordata_raw$sensor), !anyNA(colordata_raw$chip))

### Outlier report for catching possibly misentered color sensor values
outlier_report <- colordata_raw %>%
  group_by(sensor, chip) %>%
  mutate(across(c(L, a, b), \(v) v - stats::median(v), .names = "d{.col}")) %>%
  ungroup() %>%
  mutate(dist = sqrt(dL^2 + da^2 + db^2)) %>%
  filter(dist > 2) %>%
  arrange(desc(dist)) %>%
  select(sensor, replicate, chip, L, a, b, dist)

if (nrow(outlier_report) > 0L) {
  message(
    "\n",
    nrow(outlier_report),
    " replicate reading(s) sit >2 dEab units from their chip median:\n"
  )
  print(as.data.frame(outlier_report), row.names = FALSE)
}

### Create the aggregated colordata
colordata <- colordata_raw %>%
  summarise(across(c(L, a, b), mean), .by = c(chip, sensor)) %>%
  arrange(sensor, chip)

### Create the metadata data.frame
metadata <- read_excel(raw_data_path, sheet = "Metadata", range = "A1:F5") %>%
  mutate(
    Sensor = unname(c(
      VeyKolor = "veykolor",
      Nix = "nix",
      ColorMuse = "colormuse",
      KonicaMinolta = "konicaminolta"
    )[Sensor])
  ) %>%
  rename_with(str_to_lower) %>%
  mutate(observer = as.integer(observer))

stopifnot(
  !anyNA(metadata$sensor),
  setequal(metadata$sensor, unique(colordata$sensor))
)

usethis::use_data(colordata_raw, overwrite = TRUE)
usethis::use_data(colordata, overwrite = TRUE)
usethis::use_data(metadata, overwrite = TRUE)
