#' Rowwise Cosine Calculations
#'
#' Details here
#'
#' @name rowwise_cosine_simil
#' @param targetdf a data frame with long list of target words.
#' @param lookupdb a data frame containing ratings
#' @export readin
#' @import dplyr
#'
rowwise_cosine_simil <- function(targetdf = x, lookupdb = wordvec, colname1 = word, colname2 = word){

  message("Isolating join columns")
  joining_df<- targetdf %>% dplyr::mutate(joincol = colname2) #make a new column names joincol to
  joincol_df<- joining_df %>% dplyr::select(joincol)
  joining_wr <- lookupdb %>% dplyr::mutate(joincol = colname2)

  message("Joining data")

  joined <-dplyr::left_join(joincol_df, joining_wr, by="joincol") #joins embeddings to lemmas

  COSINE2 <- function(x){
    data_num <- dplyr::select_if(x, is.numeric)
    n <- nrow(data_num)
    result <- (rowSums(data_num[-1,]*data_num[-n,]))/(sqrt(rowSums(data_num[-1,]^2))*sqrt(rowSums(data_num[-n,]^2)))
    datwvpairs <- data.frame(cosine.dist = (result))
    x <- rep(NA, ncol(datwvpairs))
    datwvpairs_row1 <- rbind(x, datwvpairs)
  }

  message("Calculating pairwise cosine similarities")

  cosine_df <-COSINE2(joined) #run cosine similarity on word to word pairs

  message("Writing output dataframe")

  output_df<-merge(joining_df, cosine_df, by=0) #joins embeddings to lemmas
  output_df_clean <- dplyr::select(output_df, -c(Row.names, joincol))

  return(as_tibble(output_df_clean))
}

