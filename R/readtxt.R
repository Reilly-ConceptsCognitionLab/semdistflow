#' Read in Text
#'
#' Read in .txt documents from a specified folder found in the current working directory.
#'
#' @name readtxt
#' @return the function \code{readtxt} with return a single dataframe of text documents and their document ID
#' @importFrom data.table setattr
#' @importFrom here here
#' @export readtxt

readtxt <- function() {
  file_list <- list.files(path = here("mytexts/"), pattern = "*.txt", recursive = FALSE, full.names = TRUE) #list files with .txt ending

  textdata <- lapply(file_list, function(x) {
    paste(readLines(x), collapse=" ")
  }) #apply readlines over the file list

  data.table::setattr(textdata, "names", file_list) #add names attribute to textdata from file_list

  lapply(names(file_list), function(x){
    lapply(names(file_list[[x]]), function(y) setattr(DT[[x]], y, file_list[[x]][[y]]))
  }) #set names attribute over the list

  df1 <- data.frame(doc_id = rep(names(textdata), lengths(textdata)), doc_text = unlist(textdata), row.names = NULL) #convert to dataframe where names attribute is doc_id and textdata is text

  return(df1)
}
