#' Extract questions from QSF file
#'
#' @param qsf qsf file as R list
#' @param df convert to a \code{data.frame}? (default: TRUE)
#'
#' @details
#' This function extracts all questions from a QSF file and can convert those to a \code{data.frame}.
#'
#' @return If \code{df=FALSE} the function pulls all questions from the qsf without further modifications. If \code{df=TRUE} the function returns a \code{data.frame} with columns as listed below. Note, that the conversion into a \code{data.frame} does not preserve information such as display and skip logic.
#'
#' \itemize{
#' 	QID: Question ID (newly generated based on the sequence of questions in \code{qsf})
#' 	ID: Choice ID (if Class="Question" then ID=0)
#'  Display : Displayed text
#' 	Class: One of three values: Question, Choices, Answers.
#' 	Language : The language code of the question, choice or answer.
#' 	QuestionType : Value from [['Payload']][['QuestionType']]
#' 	PrimaryAttribute: Value from [['PrimaryAttribute']]
#' 	SecondaryAttribute: Value from [['SecondaryAttribute']]
#' 	TextEntry: Text-entry allowed?
#' 	}
#'
#'
#'
#'
#' @export
get_questions <- function(qsf, df=TRUE){
  l <- purrr::pluck(qsf, 'SurveyElements')
  if( df==TRUE){
    l <- purrr::map(purrr::keep(l, has_element_sq ), sq_as_df )
    return( dplyr::bind_rows(l, .id='QID') )
  } else {
    return( purrr::keep(l, has_element_sq ) )
  }
}

sq_as_df <- function(l){
  en <- get_q(purrr::pluck(l, "Payload") )
  en <- tibble::add_column(en, Language='EN')
  lng <- purrr::pluck(l, "Payload", "Language")
  df <- dplyr::bind_rows( map(lng, ~get_q(.)), .id="Language")
  df <- dplyr::bind_rows(en,df)
  or <- get_o( purrr::pluck(l, "Payload") )
  df <- dplyr::full_join(df, or, by=c("value", "Class"))
  df <- dplyr::select(df, "ID", dplyr::everything(), -"value")
  df <- tibble::add_column(df,
                   QuestionType=purrr::pluck(l, "Payload", "QuestionType"),
                   PrimaryAttribute=purrr::pluck(l, "PrimaryAttribute"),
                   SecondaryAttribute=purrr::pluck(l, "SecondaryAttribute"))
  return(df)
}

# Get question, choices, answers
get_q <- function(l){
  df <- tibble::tibble(value="0", Display=purrr::pluck(l, "QuestionText"), Class="Question")
  choices <- dplyr::bind_rows(purrr::map(purrr::pluck(l, "Choices"), tibble::as_tibble), .id='value')
  choices <- dplyr::mutate(choices, Class='Choices')
  if( purrr::vec_depth(purrr::pluck(l, "Answers"))>3 ) {
    warning("Skipping Answers for special matrix questions.")
    return(dplyr::bind_rows(df, choices))
  }
  answers <- dplyr::bind_rows(purrr::map(purrr::pluck(l, "Answers"), tibble::as_tibble), .id='value')
  answers <- dplyr::mutate(answers, Class='Answers')
  df <- dplyr::bind_rows(df, choices, answers)
  return(df)
}

# Get order of choices and answers
get_o <- function(l){
  df <- tibble::tibble(value="0", ID="0", Class="Question")
  choices <- purrr::map(purrr::pluck(l, "ChoiceOrder"), as.character)
  answers <- purrr::map(purrr::pluck(l, "AnswerOrder"), as.character)
  choices <- dplyr::bind_rows(purrr::map(choices, tibble::as_tibble), .id='ID')
  answers <- dplyr::bind_rows(purrr::map(answers, tibble::as_tibble), .id='ID')
  choices <- dplyr::mutate(choices, Class='Choices')
  answers <- dplyr::mutate(answers, Class='Answers')
  df <- dplyr::bind_rows(df, choices, answers)
  return(df)
}
