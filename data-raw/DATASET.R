library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)

### Reorder native `wide` layout of colorchip and readings to `long` layout
format_data_sheet <- function(sheet) {
  names(sheet) <- col_names

  sheet %>%
    pivot_longer(
      cols = everything(),
      names_to = c(".value", "hue"),
      names_pattern = "^(chip|L|a|b)(\\d+)$"
    ) %>%
    select(-hue)
}

### Read in the color data sheets to build the main color data sets (ignoring computed `Mean` sheets)
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

colordata_raw <- raw_data_path %>%
  excel_sheets() %>%
  str_subset("Mean", negate = TRUE) %>%
  head(12) %>%
  set_names() %>%
  map(\(s) {
    read_excel(raw_data_path, sheet = s) %>%
      select(1:12) %>%
      format_data_sheet()
  }) %>%
  list_rbind(names_to = "sheet") %>%
  mutate(
    replicate = str_extract(sheet, "(?<=NewBook_)\\d+") %>% as.integer(),
    sensor = str_extract(sheet, "[^_]+$"),
    .before = 1
  ) %>%
  mutate(
    sensor = recode_values(
      sensor,
      "VK" ~ "veykolor",
      "Nix" ~ "nix",
      "CM" ~ "colormuse",
      "CR400" ~ "konicaminolta"
    )
  ) %>%
  select(-sheet)

### Create the aggregated colordata
colordata <- colordata_raw %>%
  summarise(across(c(L, a, b), mean), .by = c(chip, sensor))

usethis::use_data(colordata_raw, overwrite = TRUE)
usethis::use_data(colordata, overwrite = TRUE)
