#' Create O-D matrix from flight passenger numbers
#'
#' @param flight_data The dataframe containing the flight passenger numbers.
#' @param flight_month The month (or months) that the flight data used to construct
#' the O-D matrix should cover.
#' @param flight_year The year (or years) that the flight data used to construct
#' the O-D matrix should cover.
#'
#' @return Matrix in which each cell represents the probability that a person
#' leaving the origin country (the rows of the matrix) flies to a given destination
#' (the columns of the matrix).
#' @export
#'
od_matrix <- function(flight_data, flight_month, flight_year) {

  iata_selection <- flight_data %>%
    dplyr::group_by(orig_country) %>%
    dplyr::filter(dest_country != orig_country) %>%
    dplyr::filter(month %in% flight_month & year %in% flight_year) %>%
    dplyr::mutate(passenger_proportion = passengers/sum(passengers))

  matrix <- iata_selection %>%
    tidyr::pivot_wider(id_cols = orig_country,
                names_from = dest_country,
                values_from = passenger_proportion) %>%
    dplyr::select(orig_country, order(colnames(.))) %>%
    replace(is.na(.), 0)

  if(ncol(matrix) != (nrow(matrix) + 1)) {
    a <- unique(matrix$orig_country)
    b <- colnames(matrix[2:ncol(matrix)])

    missing_dest <- setdiff(a,b)

    matrix[, paste(missing_dest)] <- 0

  }

  matrix <- matrix %>%
    dplyr::select(orig_country, order(colnames(.)))
}
