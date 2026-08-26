#' Raw Color Data
#'
#' This data.frame contains all of the individual color scan measurements of
#' three brand-new Munsell Soil Color books (2.5Y, 10YR, and 7.5YR only for now).
#' This dataset is not directly used in this package, and is only included for the
#' user to inspect the quality of the measurements.
#'
#' @format A data.frame with 1224 rows and 6 variables
#' \describe{
#' \item{replicate}{The replicate/book number}
#' \item{sensor}{The color sensor name}
#' \item{chip}{The Munsell notation of the color chip}
#' \item{L}{CIELab L value}
#' \item{a}{CIELab a value}
#' \item{b}{CIELab b value}
#' }
#'
#' @source TBD
"colordata_raw"

#' Color Data
#'
#' This data.frame contains the aggregated color scan measurements of
#' three brand-new Munsell Soil Color books (2.5Y, 10YR, and 7.5YR only for now).
#' This dataset is used for comparisons with user scans, and is treated as the 'Truth'
#' when judging user books.
#'
#' @format A data.frame with 412 rows and 6 variables
#' \describe{
#' \item{sensor}{The color sensor name}
#' \item{chip}{The Munsell notation of the color chip}
#' \item{L}{CIELab L value}
#' \item{a}{CIELab a value}
#' \item{b}{CIELab b value}
#' }
#'
#' @source TBD
"colordata"
