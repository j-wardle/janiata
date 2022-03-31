#' Import IATA data
#'
#' @param filename The file path that should be imported
#'
#' @return A dataframe.
#' @export
#'
import_iata <- function(filename) {

  if (tools::file_ext(filename) == "tsv") {

    x <- readr::read_tsv(filename,
                  skip = 10,
                  col_types = readr::cols(
                    `Orig Country` = readr::col_character(),
                    Orig = readr::col_character(),
                    `Stop #1` = readr::col_character(),
                    `Stop #2` = readr::col_character(),
                    `Stop #3` = readr::col_character(),
                    `Stop #4` = readr::col_character(),
                    `Stop #5` = readr::col_character(),
                    `Dest Country` = readr::col_character(),
                    Dest = readr::col_character(),
                    `Reported Pax` = readr::col_double(),
                    `Reported + Est. Pax` = readr::col_double(),
                    `Pax Share` = readr::col_double(),
                    Fare = readr::col_double(),
                    Est. = readr::col_character(),
                    Revenue = readr::col_double(),
                    Yield = readr::col_double(),
                    RPM = readr::col_double()
                  )
    )
  }

  if (tools::file_ext(filename) == "xls") {

    message("reading xls file")
    x <- readxl::read_excel(filename,
                    skip = 8
    )

  }

  records <- nrow(x)
  x <- dplyr::slice(x, -(records - 16):- records)
  x <- tidyr::drop_na(x, Orig)
  x <- janitor::clean_names(x)

}
