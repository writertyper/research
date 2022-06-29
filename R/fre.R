
#' Get labels from a dataset
#'
#' @param .data A dataset.
#' @param var A variable.
#' @return The frequency of a variable.





fre <- function(.data, var) {
  .data %>%
    haven::as_factor() %>%
    janitor::tabyl(dplyr::all_of(var)) %>%
    janitor::adorn_totals() %>%
    tibble::as_tibble()
}
