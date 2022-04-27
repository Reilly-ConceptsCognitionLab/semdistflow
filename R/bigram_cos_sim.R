#' Rowwise Cosine Calculations
#'
#' Details here
#'
#' @name bigram_cos_sim
#' @param targetdf a dataframe with long list of target words
#' @param lookupdb a dataframe containing ratings
#' @param colname1 the column name from targetdf used to join
#' @param colname2 the column name from lookupdb used to join
#' @param flipped choose to flip direction of cosine similarity
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr left_join
#' @importFrom raster rowSums
#' @importFrom jmvcore enquo
#' @importFrom magrittr %>%
#' @export bigram_cos_sim
#
bigram_cos_sim <- function(targetdf = x, lookupdb = wordvec, colname1 = NULL, colname2 = NULL, flipped = FALSE){
  col1 <- enquo(colname1) #renames column1 to col1 and allows you to enter the column name in the function without "quotations"
  col2 <- enquo(colname2)
  #various embeddings can be found at https://osf.io/ryhfj/

  message("Isolating join columns")
  joining_df<- targetdf %>% dplyr::mutate(joincol = !!col1) #make a new column in targetdf named joincol to
  joining_df <- joining_df %>% dplyr::select(doc_id, everything()) #move doc_id to the first column
  joincol_df<- joining_df %>% dplyr::select(doc_id, joincol) #select doc_id and joincol
  joining_wr <- lookupdb %>% dplyr::mutate(joincol = !!col2) #make a new column in lookupdb names joincol to

  message("Joining data + print")

  joined <-dplyr::left_join(joincol_df, joining_wr, by="joincol") #joins embeddings to based on joincol

  COSINE2 <- function(x){
    data_num <- dplyr::select_if(x, is.numeric) #select all columns that are numeric
    n <- nrow(data_num) # identify length of the df
    result <- (raster::rowSums(data_num[-1,]*data_num[-n,]))/(sqrt(raster::rowSums(data_num[-1,]^2))*sqrt(raster::rowSums(data_num[-n,]^2)))
    # calculate cosine; data_num[-1,] (dat_num minus the first row) and data_num[-n,] (dat_num minus the last row), lagging the df
    datwvpairs <- data.frame(cosine.dist = (result)) #convert vector to df
    x <- rep(NA, ncol(datwvpairs)) #add back in a NA row so that datwvpairs and x are same length
    datwvpairs_row1 <- rbind(x, datwvpairs)
  }

  message("Calculating pairwise cosine similarities")

  by_id <- joined %>% group_by(doc_id) #group df
  cosine_df<-by_id %>% group_modify(~ COSINE2(.))
   #run cosine similarity on word to word pairs using group_modify()

  message("Writing output dataframe")

  output_df<-merge(joining_df, cosine_df, by=0:1, sort = FALSE) #joins embeddings to wordlist
  output_df_lag <- output_df %>%
    group_by(doc_id) %>%
    mutate(., pair = paste(lag({{colname1}}), {{colname1}}, sep="-"), .after=({{colname1}})) #add lagged wordlist
  output_df_clean <- dplyr::select(output_df_lag, -c(Row.names, joincol)) #remove extra columns

 #doesnt flip cosine
  if (flipped == FALSE) {
    return(as_tibble(output_df_clean))
  }

  #flip cosine
  if (flipped == TRUE) {
    output_df_flipped <- output_df_clean %>%
      mutate(flipped_cosine.dist = 1-cosine.dist)
    return(as_tibble(output_df_flipped))
  }


}
