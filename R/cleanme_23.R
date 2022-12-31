#' Clean Raw Text
#'
#' Use helper function to clean raw text
#'
#' @param x A dataframe with raw text in a column titled "doc_text".
#' @return the function \code{cleanme} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @importFrom tibble as_tibble
#' @keywords internal

cleanme <- function(dat){
  message("Performing text cleaning.... sit tight!")
  doc_clean <- dat %>% group_by(doc_id) %>% dat$doc_text %>% dplyr::group_modify(~ cleantxt(.))
  #isolate the text column apply the cleantxt function by doc_id
  final <- cbind(x,doc_clean) #add doc_clean column to original dataframe
  return(as_tibble(final))
}

devtools::document()
