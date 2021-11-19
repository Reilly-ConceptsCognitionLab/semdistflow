#' Import dplyr
rowwise_cosine_simil <- function(data_file = x, word_rating = wordvec, colname1 = word, colname2 = word){

  joining_df <- data_file %>% dplyr::mutate(joincol = colname1) #turns blinks into NA for interpolation
  joining_wr <- word_rating %>% dplyr::mutate(joincol = colname2)

  joined <-dplyr::left_join(joining_df, joining_wr, by="joincol") #joins embeddings to lemmas

  COSINE2 <- function(x){
    data_num <- dplyr::select_if(x, is.numeric)
    n <- nrow(data_num)
    result <- (rowSums(data_num[-1,]*data_num[-n,]))/(sqrt(rowSums(data_num[-1,]^2))*sqrt(rowSums(data_num[-n,]^2)))
    datwvpairs <- data.frame(cosine.dist = (result))
    x <- rep(NA, ncol(datwvpairs))
    datwvpairs_row1 <- rbind(x, datwvpairs)
  }

  cosine <- COSINE2(joined) #run cosine similarity on word to word pairs

  return(as_tibble(cosine))
}
