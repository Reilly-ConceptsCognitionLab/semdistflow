#' Rowwise Euclidean Distance
#'
#'
#' @name rowwise_euc_diff
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr left_join
#' @export rowwise_euc_diff
rowwise_euc_diff <- function(data_file = x, word_rating = wordvec, colname1 = word, colname2 = word){

  message("Isolating join columns")
  joining_df<- data_file %>% dplyr::mutate(joincol = colname2) #make a new column names joincol to
  joincol_df<- joining_df %>% dplyr::select(joincol)
  joining_wr <- word_rating %>% dplyr::mutate(joincol = colname2)

  message("Joining data")

  joined <-dplyr::left_join(joincol_df, joining_wr, by="joincol") #joins embeddings to lemmas

  EUC2 <- function(x){
    data_num <- dplyr::select_if(x, is.numeric)
    n <- nrow(data_num)
    result <- sqrt(rowSums((data_num[-1, ] - data_num[-n, ])^2))
    dat15pairs <- data.frame(sd15 = (result))
    x <- rep(NA, ncol(dat15pairs))
    dat15pairs_row1 <- rbind(x, dat15pairs)
  }

  message("Calculating pairwise euclidian diffs")

  euc_df <-EUC2(joined) #run cosine similarity on word to word pairs

  message("Writing output dataframe")

  output_df<-merge(joining_df, euc_df, by=0) #joins embeddings to lemmas
  output_df_clean <- dplyr::select(output_df, -c(Row.names, joincol))

  return(as_tibble(output_df_clean))
}


