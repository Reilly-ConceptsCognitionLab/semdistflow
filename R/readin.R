#' Read in Text
#'
#' Read in .txt documents from a specified folder in the working directory
#'
#' @param folder_name A string.
#' @param y A number.
#' @return Dataframe of text documents and their document ID
#' @examples
#' readin("text")
#' readin("data")


readin <- function(folder_name = "data"){

  file_list <- list.files(folder_name, pattern = "*.txt", recursive = TRUE, full.names = TRUE)

  textdata <- lapply(file_list, function(x) {
    paste(readLines(x), collapse=" ")
  })

  setattr(textdata, "names", file_list)

  df1 <- data.frame(doc_id = rep(names(textdata), lengths(textdata)), doc_text = unlist(textdata), row.names = NULL)

  return(as_tibble(df1))
}
