#' Clean Raw Text keeping pronouns
#'
#' Use helper functions to clean raw text
#'
#' @param x A dataframe with raw text in a column titled "doc_text".
#' @return the function \code{clean_df_nopronouns} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @importFrom tibble as_tibble
#' @export

clean_df_nopronouns <- function(x){

  rawdat <- x$doc_text
  clean1dat <- clean1(rawdat)
  clean2dat <-clean2_nopronouns(clean1dat)
  doc_clean<- clean3(clean2dat)

  final <- cbind(x,doc_clean)

  return(as_tibble(final))

}
