
#' Get labels from a dataset
#'
#' @param .data A dataset.
#' @param .var A variable.
#' @return The frequency of a variable.
#' @examples
#' fre(mtcars, "carb")




fre <- function(.data, var) {
  .data %>%
    as_factor() %>%
    tabyl(all_of(var)) %>%
    adorn_totals() %>%
    as_tibble()
}
