#' Import multiple IATA datasets at a national level
#'
#' @param analysis_years The years for which IATA data should be imported.
#' @param folder The file path for the folder where IATA data are saved.
#'
#' @return A dataframe formed from the flight passenger numbers in all the
#' imported datasets. Flight passenger numbers are aggregated to a national
#' level in which the origin and destination locations are countries (rather
#' than individual airports as in the raw IATA data).
#' @export
#'
#' @examples analysis_years <- 2012:2014
#' @examples folder <- "~/iata_analysis/data/"
#' @examples import_iata_multi_nat(analysis_years, folder)
import_iata_multi_nat <- function(analysis_years, folder) {

  iata_multi <- purrr::map(analysis_years, function(year) {

    data_folder <- paste0(folder, year, "/")
    data_files <- list.files(data_folder)
    data_files <- paste0(data_folder, data_files)

    iata_data <- purrr::map(data_files, function(x) {

      out <- import_iata(x)

    })

    # Name list elements with file abbreviation
    names(iata_data) <- tools::file_path_sans_ext(list.files(data_folder))

    iata_data <- dplyr::bind_rows(iata_data, .id = "file")

    iata_data <- iata_data %>%
      dplyr::mutate(year = stringr::str_sub(file, start=1, end=4),
             month = stringr::str_sub(file, start = 5, end = 6))

    iata_data <- iata_data %>%
      dplyr::group_by(year, month, orig_country) %>%
      dplyr::count(dest_country, wt = reported_est_pax, name = "passengers")

  })

  names(iata_multi) <- analysis_years
  iata_multi <- dplyr::bind_rows(iata_multi)

}
