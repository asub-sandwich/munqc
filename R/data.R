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
#' @format A data.frame with 412 rows and 5 variables
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

#' Chip Finish
#'
#' This data.frame contains the percepted "finish" of the chips scanned
#' from the reference munsell soil color books. The manufacturer of these
#' books can finish the chip in one of `matte`, `semigloss`, or `gloss`.
#' Due to limitations in most colorimeters, it is difficult to measure glossy
#' chips accurately and repeatably. Further, it would seem these chips may
#' degrade the quickest. For these reasons, the finish data is included to
#' help users make appropriate decisions regarding the quality of their color books.
#' Users may provide their own glossiness data if their books finishes differ from
#' this dataset.
#'
#' @format a data.frame with 102 rows and 2 variables
#' \describe{
#' \item{chip}{The Munsell notation of the color chip}
#' \item{finish}{The percieved colorchip finish}
#' }
#'
#' @source TBD
"chipfinish"
