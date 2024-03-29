#' Rowwise Cosine Distance Calculations
#'
#' Details here
#'
#' @name distme
#' @param targetdf a dataframe representing a list of words under the column header doc_clean
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom textstem lemmatize_words
#' @importFrom tidytext unnest_tokens
#' @importFrom lsa cosine
#' @importFrom here here
#' @importFrom magrittr %>%
#' @importFrom tibble as.tibble
#' @export distme

#input the object from the cleanme() steps
distme <- function(targetdf, lemmatize=TRUE){
  message("Loading lookup databases and joining your data to SemDist15 and Glowca")
  #load lookup databases
  glowca_v1 <- glowca_vol1_2023 #assumes package lazyloads the datasets
  glowca_v2 <- glowca_vol2_2023
  sd15 <- semdist15_2023
  glowca <-  rbind(glowca_v1, glowca_v2)
  dat <- targetdf
  #groups by factor variables and unlists the string, one word per row
  dat <- dat %>% group_by(doc_id, doc_text) %>% tidytext::unnest_tokens(word, doc_clean)
  if (lemmatize == TRUE) {
    #lemmatizes target dataframe on column labeled 'lemma1'
    dat2 <- dat %>% mutate(lemma1 = textstem::lemmatize_words(word))
    #join data to lookup databases specifying common join name explictly
    joindf_semdist15 <- dplyr::left_join(dat2, sd15, by=c("lemma1"="word"))
    joindf_glowca <- dplyr::left_join(dat2, glowca, by=c("lemma1"="word"))
    #Select numeric columns dplyr fn operates on tibbles
    dat_sd15_onlynumeric <- tibble::as.tibble(joindf_semdist15) %>% dplyr::select_if(is.numeric)
    dat_glo_onlynumeric <- tibble::as.tibble(joindf_glowca) %>% dplyr::select_if(is.numeric)
    #convert join dataframes containing hyperparameter values (glowca and sd15) to matrices
    mat_sd15 <- data.matrix(dat_sd15_onlynumeric)
    mat_glo <- data.matrix(dat_glo_onlynumeric)
    #compute cosine distance for each running pair of words in sd15 and glowca
    vals_sd15 <- unlist(lapply(2:nrow(mat_sd15), function(i){lsa::cosine(mat_sd15[i-1,],     mat_sd15[i,])}))
    vals_glo <- unlist(lapply(2:nrow(mat_glo), function(i){lsa::cosine(mat_glo[i-1,], mat_glo[i,])}))
    vals_sd15 <- data.frame(vals_sd15)
    vals_glo <- data.frame(vals_glo)
    #Rename first column of cosine distance values
    names(vals_sd15)[1] <- "Sd15_Cosine"
    names(vals_glo)[1] <- "Glo_Cosine"
    #Add NA to final row to match length, there is no pairwise distance for final observation
    vals_sd15[nrow(vals_sd15)+1, ] <- NA
    vals_glo[nrow(vals_glo)+1, ] <- NA
    #Reverse scale the cosine values for gloca and sd15, subtract each obs from max (1)
    sd15vals <- vals_sd15 %>% mutate(Sd15_CosRev0Score= 1-Sd15_Cosine)
    glowcavals <- vals_glo %>% mutate(Glowca_CosRev0Score = 1-Glo_Cosine)
    #Rebuild the dataframe creating bigram columns, cbind words to their cosine values
    dat3<- dat2 %>% ungroup() %>% mutate(lemma2 = lead(lemma1, 1)) #rebuild dataframe, cast lead (+1) bigram
    #Bind the sd15 cosine data to the original dataframe
    mydists_lemmatized <- cbind(dat3, sd15vals, glowcavals)
    #Make last word of each text NA so we don't include cosine calculations between texts
    done <- mydists_lemmatized %>% group_by(doc_id) %>%
      mutate(lemma2 = replace(lemma2, n(), NA)) %>%
      mutate(Sd15_Cosine = replace(Sd15_Cosine, n(), NA)) %>%
      mutate(Sd15_CosRev0Score = replace(Sd15_CosRev0Score, n(), NA)) %>%
      mutate(Glo_Cosine = replace(Glo_Cosine, n(), NA)) %>%
      mutate(Glowca_CosRev0Score = replace(Glowca_CosRev0Score, n(), NA)) %>% ungroup
    return(as_tibble(done))
  }

  if (lemmatize == FALSE) {
    message("Loading lookup databases and joining your data to SemDist15 and Glowca")
    dat2 <- dat %>% mutate(word1 = word)
    #join on the unlemmatized dataframe with
    joindf_semdist15 <- dplyr::left_join(dat, sd15, by=c("word1"="word"))
    joindf_glowca <- dplyr::left_join(dat, glowca, by=c("word1"="word"))
    #Select numeric columns for cosine calculations, eliminate columns with string data
    dat_sd15_onlynumeric <- tibble::as.tibble(joindf_semdist15) %>% dplyr::select_if(is.numeric)
    dat_glo_onlynumeric <- tibble::as.tibble(joindf_glowca) %>% dplyr::select_if(is.numeric)
    #convert join dataframes containing hyperparameter values (glowca and sd15) to matrices
    mat_sd15 <- data.matrix(dat_sd15_onlynumeric)
    mat_glo <- data.matrix(dat_glo_onlynumeric)
    message("Computing Distances.... Be patient!!!")
    #convert join dataframes containing hyperparameter values (glowca and sd15) to matrices
    mat_sd15 <- data.matrix(dat_sd15_onlynumeric)
    mat_glo <- data.matrix(dat_glo_onlynumeric)
    #compute cosine distance for each running pair of words in sd15 and glowca
    vals_sd15 <- unlist(lapply(2:nrow(mat_sd15), function(i){lsa::cosine(mat_sd15[i-1,],     mat_sd15[i,])}))
    vals_glo <- unlist(lapply(2:nrow(mat_glo), function(i){lsa::cosine(mat_glo[i-1,], mat_glo[i,])}))
    vals_sd15 <- data.frame(vals_sd15)
    vals_glo <- data.frame(vals_glo)
    #Rename first column of cosine distance values
    names(vals_sd15)[1] <- "Sd15_Cosine"
    names(vals_glo)[1] <- "Glo_Cosine"
    #Add NA to final row to match length, there is no pairwise distance for final observation
    vals_sd15[nrow(vals_sd15)+1, ] <- NA
    vals_glo[nrow(vals_glo)+1, ] <- NA
    #Reverse scale the cosine values for gloca and sd15, subtract each obs from max (1)
    sd15vals <- vals_sd15 %>% mutate(Sd15_CosRev0Score= 1-Sd15_Cosine)
    glowcavals <- vals_glo %>% mutate(Glowca_CosRev0Score = 1-Glo_Cosine)
    #Rebuild the dataframe creating bigram columns, cbind words to their cosine values
    dat3<- dat2 %>% ungroup() %>% mutate(word2 = lead(word1, 1)) #rebuild dataframe, cast lead (+1) bigram
    #Bind the sd15 cosine data to the original dataframe
    mydists_unlemmatized <- cbind(dat3, sd15vals, glowcavals)
    done <-  mydists_unlemmatized %>% group_by(doc_id) %>%
      mutate(word2 = replace(word2, n(), NA)) %>%
      mutate(Sd15_Cosine = replace(Sd15_Cosine, n(), NA)) %>%
      mutate(Sd15_CosRev0Score = replace(Sd15_CosRev0Score, n(), NA)) %>%
      mutate(Glo_Cosine = replace(Glo_Cosine, n(), NA)) %>%
      mutate(Glowca_CosRev0Score = replace(Glowca_CosRev0Score, n(), NA)) %>% ungroup
    #output a formatted tibble
    return(as_tibble(done))
  }
}

