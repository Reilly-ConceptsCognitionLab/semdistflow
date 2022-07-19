#' Applies cleaning function to multiple texts
#'
#' Uses helper functions to clean raw text
#'
#' @param x A dataframe with of mutiple documents denoted in a column titled "doc_id" and raw text in a column titled "doc_text" .
#' @return the function \code{cleanme} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @importFrom tibble as_tibble
#' @import stringi
#' @import tidyverse
#' @export

cleanme <- function(datafile, replace_pronoun = FALSE){

  if (replace_pronoun == FALSE) {
    message("Performing cleaning retaining pronouns")
  output <- datafile %>%
    group_by(doc_id) %>%
    dplyr::group_modify(~ clean_df_nopronouns(.))

  return(output)

  }

  if (replace_pronoun == TRUE) {
    message("Performing cleaning replacing pronouns")
    output <- datafile %>%
      group_by(doc_id) %>%
      dplyr::group_modify(~ clean_df(.))

    return(output)

}

}

