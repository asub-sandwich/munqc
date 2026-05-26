library(tidyverse)

# DOCUMENT LATER
new_scan_collection <- function(
  data,
  sensor = c("veykolor", "nix", "colormuse", "konicaminolta"),
  illuminant = c("D65", "C"),
  observer = c(10, 2)
) {
  sensor <- match.arg(sensor)
  illuminant <- match.arg(illuminant)
  observer <- match.arg(as.character(observer), c("10", "2")) %>% as.integer()

  stopifnot(is.data.frame(data))
  stopifnot(all(c("chip", "L", "a", "b") %in% names(data)))

  structure(
    list(
      data = data,
      sensor = sensor,
      illuminant = illuminant,
      observer = observer,
      ...
    ),
    class = "ScanCollection"
  )
}

print.ScanCollection <- function(x, ...) {
  cat("ScanCollection\n")
  cat("Sensor:", x$sensor, "\n")
  cat("Chips: ", nrow(x$data), "\n")
  print(head(x$data, 10))
}
