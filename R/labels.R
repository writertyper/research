#' Get labels from a dataset
#'
#' @param .data A dataset.
#' @return The labels of a dataset.
#' @examples
#' labels(mtcars)


labels <- function(.data) {
  .data %>%
    purrr::map_dfc(attr, "label") %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name") %>%
    dplyr::rename(`label` = 1)
}
