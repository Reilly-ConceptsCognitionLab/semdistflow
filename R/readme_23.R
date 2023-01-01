#' Read in Text
#'
#' Read in .txt documents from a specified folder found in the current working directory.
#'
#' @name readme
#' @param folder_name  string denoting where user's text files are located ('my_texts' is good)
#' @return the function \code{readme} with return a single dataframe of text documents and their document ID
#' @examples
#' readtxt("text")
#' readtxt("data")
#' @importFrom data.table setattr
#' @export readme


readme <- function(folder_name = "my_texts"){
  file_list <- list.files(path = folder_name, pattern = "*.txt", recursive = TRUE, full.names = TRUE) #list files with .txt ending
  textdata <- lapply(file_list, function(x) {
    paste(readLines(x), collapse=" ", warn=F)
  }) #apply readlines over the file list, no warning if last line is not complete

  data.table::setattr(textdata, "names", file_list) #add names attribute to textdata from file_list

  lapply(names(file_list), function(x){
    lapply(names(file_list[[x]]), function(y) setattr(DT[[x]], y, file_list[[x]][[y]]))
  }) #set names attribute over the list

  df1 <- data.frame(doc_id = rep(names(textdata), lengths(textdata)), doc_text = unlist(textdata), row.names = NULL) #convert to dataframe where names attribute is doc_id and textdata is text

  return(df1)
}
