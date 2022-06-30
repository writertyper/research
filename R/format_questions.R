
#' Format questions to prepare for export
#'
#' @param .data A qsf dataframe
#' @param language language of survey
#' @param raw raw includes Answers Class and no spaces; default set to FALSE
#' @return formatted dataframe ready for export


format_questions <- function(.data, language, raw = FALSE) {



  # TO DO: Split chunks based on mutate, filter, arrange, etc.
  a <- .data %>%
    dplyr::filter(Language == language) %>%
    dplyr::group_by(QID) %>%
    dplyr::mutate(Display = ifelse(Class == "Choices", paste0("&emsp;", Display), Display))  %>%
    dplyr::mutate(Display = stringr::str_remove_all(Display, "<[^>]+>"))  %>%
    dplyr::mutate(Display = stringr::str_replace_all(Display, "&nbsp;", " ")) %>%
    dplyr::mutate(Display = stringr::str_replace(Display, "&emsp;", "    ")) %>%
    dplyr::arrange(DataExportTag)

  # Filter by Answers and add concatanated values in the Display column
  b <- a %>%
    dplyr::group_by(QID, DataExportTag) %>%
    dplyr::filter(Class == "Answers") %>%
    dplyr::group_modify(~dplyr::add_row(.x, Display=paste(.x$Display, collapse = "; "), Class = "Options"))


  b <- b %>%
    dplyr::filter(Class == "Options")

  c <- a %>%
    dplyr::bind_rows(b) %>%

    dplyr::arrange(QID) %>%
    dplyr::arrange(DataExportTag) %>%
    dplyr::mutate(Class_ID = dplyr::case_when(Class == "Question" ~ 1,
                                Class == "Options" ~ 2,
                                Class == "Choices" ~ 3)) %>%
    dplyr::mutate(ID = as.numeric(ID)) %>%
    dplyr::arrange(DataExportTag, Class_ID, ID)

  if(raw == TRUE) {

#    print("raw is true")

  } else if(raw == FALSE) {
    c <- c %>%
      dplyr::mutate(Format = NA) %>%
      dplyr::filter(Class != "Answers") %>%
      dplyr::arrange(DataExportTag, Class_ID, ID) %>%
      dplyr::group_by(DataExportTag) %>%
      dplyr::group_modify(~dplyr::add_row(.x, Format = "-")) %>%
      dplyr::group_modify(~dplyr::add_row(.x, Format = "-"))

 #   print("raw is false")

  }




  return(c)

}
