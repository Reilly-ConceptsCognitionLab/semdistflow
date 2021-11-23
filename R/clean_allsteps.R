#' Clean Raw Text
#'
#' Use helper functions to clean raw text
#'
#' @param x A dataframe with raw text in a column titled "doc_text".
#' @return the function \code{clean_df} with return a dataframe of cleaned text documents, their document ID and the raw text.
#' @examples
#' clean_df(mydata)
#' @importFrom tibble as_tibble
#' @export

clean_df <- function(x){


  rawdat <- x$doc_text
  clean1dat <- clean1(rawdat)
  clean2dat <-clean2(clean1dat)
  doc_clean<- clean3(clean2dat)

  final <- cbind(x,doc_clean)

  return(as_tibble(final))

}
