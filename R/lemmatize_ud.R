lemmatize_ud<- function(x){
  lemma <- udpipe(testdata, modelload) #apply udpipe to each dataframe in the list
  lemmatized_nopunct<-dplyr::filter(list_lemma, upos != "PUNCT")

  return(as_tibble(lemmatized_nopunct))
}
