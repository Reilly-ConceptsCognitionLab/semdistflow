# not working
lemmatize_textstem <- function(datafile=x, colname=doc_clean){
  data <- x
  col<-colname

  data$lemma <- textstem::lemmatize_words(data$colname)

  return(as.tibble(data))
}
