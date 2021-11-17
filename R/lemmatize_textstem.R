# not working
lemmatize_textstem <- function(x){
  data$lemma <- textstem::lemmatize_words(x$doc_clean)

  return(as.tibble(data))
}



