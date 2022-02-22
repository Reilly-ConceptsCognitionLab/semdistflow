#' Applies cleaning function to multiple texts
#'
#' Uses helper functions to clean raw text
#'
#' @param x A dataframe with of mutiple documents denoted in a column titled "doc_id" and raw text in a column titled "doc_text" .
#' @return the function \code{clean_df_bygroup} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @importFrom tibble as_tibble
#' @export

clean_df_bygroup <- function(datafile){

  output <- datafile %>%
    group_by(doc_id) %>%
    group_modify(~ clean_df_nopronouns(.))

  return(output)

}
