#' Clean Raw Text keeping pronouns
#'
#' Use helper functions to clean raw text
#'
#' @param x A dataframe with raw text in a column titled "doc_text".
#' @return the function \code{clean_df_nopronouns} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @importFrom tibble as_tibble
#' @keywords internal

clean_df_nopronouns <- function(x){
  rawdat <- x$doc_text #isolate the text column
  clean1dat <- clean1(rawdat) #apply clean1 function - text cleaning
  clean2dat <-clean2_nopronouns(clean1dat) #apply clean2_nopronouns function - replacements without replacing pronouns
  doc_clean<- clean3(clean2dat) #apply clean3 function - repetitions and extra stuff

  final <- cbind(x,doc_clean) #add doc_clean column to original dataframe

  return(final)

}
