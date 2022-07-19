#' Rowwise Cosine Calculations
#'
#' Details here
#'
#' @name distme
#' @param targetdf a dataframe with long list of target words
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
#' @export distme

# Not sure what to do about text_tools and referencing that folder
#input the object from the cleanme() steps
distme <- function(targetdf, lemmatize=TRUE){
  if (lemmatize == TRUE) {
    message("Loading lookup databases and joining your data to SemDist15 and Glowca")
    #load lookup databases
    load(here("data", "glowca_lite.rda")) #rounded 5,subtlex matched 60k
    load(here("data", "semdist15_new.rda"))
    #groups by factor variables and unlists the string, one word per row
    dat <-targetdf %>% group_by(doc_id, doc_text) %>% tidytext::unnest_tokens(word, doc_clean)
    #lemmatizes target dataframe on column labeled 'lemma1'
    dat2 <- dat %>% mutate(lemma1 = textstem::lemmatize_words(word))
    #join semdist15 and glowca lookup databases with target input dataframe
    joindf_semdist15 <- left_join(dat2, semdist15_new, by=c("lemma1"="word")) %>% data.frame()
    joindf_glowca <- left_join(dat2, glowca, by=c("lemma1"="word")) %>% data.frame()
    #Select numeric columns for cosine calculations, eliminate columns with string data
    dat_sd15 <- joindf_semdist15 %>% select_if(is.numeric)
    datglo <- joindf_glowca %>% select_if(is.numeric)
    #convert join dataframes containing hyperparameter values (glowca and sd15) to matrices
    mat_sd15 <- as.matrix(dat_sd15)
    mat_glo <- as.matrix(datglo)
    #compute cosine distance for each running pair of words in sd15 and glowca
    vals_sd15 <- unlist(lapply(2:nrow(mat_sd15), function(i){
      lsa::cosine(mat_sd15[i-1,],  mat_sd15[i,])
    }))
    vals_glo <- unlist(lapply(2:nrow(mat_glo), function(i){
      lsa::cosine(mat_glo[i-1,], mat_glo[i,])
    }))
    #Convert matrices back to dataframes
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
    mydists_lemmatized %>%
      group_by(doc_id) %>%
      mutate(lemma2 = replace(lemma2, n(), NA)) %>%
      mutate(Sd15_Cosine = replace(Sd15_Cosine, n(), NA)) %>%
      mutate(Sd15_CosRev0Score = replace(Sd15_CosRev0Score, n(), NA)) %>%
      mutate(Glo_Cosine = replace(Glo_Cosine, n(), NA)) %>%
      mutate(Glowca_CosRev0Score = replace(Glowca_CosRev0Score, n(), NA)) %>%
      ungroup -> mydists_lemmatized_NAatend
    #output a formatted dataframe
    return(as_tibble(mydists_lemmatized_NAatend))
  }

  if (lemmatize == FALSE) {
    message("Loading lookup databases and joining your data to SemDist15 and Glowca")
    here::i_am()
    load(here("data", "glowca_lite.rda")) #rounded 5,subtlex matched 60k
    load(here("data", "semdist15_new.rda"))
    #groups by factor variables and unlists the string, one word per row
    dat <-targetdf %>% group_by(doc_id, doc_text) %>% tidytext::unnest_tokens(word, doc_clean)
    #join semdist15 and glowca lookup databases with target input dataframe
    joindf_semdist15 <- left_join(dat2, semdist15_new, by=c("word1"="word")) %>% data.frame()
    joindf_glowca <- left_join(dat2, glowca, by=c("word1"="word")) %>% data.frame()
    #Select numeric columns for cosine calculations, eliminate columns with string data
    dat_sd15 <- joindf_semdist15 %>% select_if(is.numeric)
    datglo <- joindf_glowca %>% select_if(is.numeric)
    message("Computing Distances.... Be patient!!!")
    #convert join dataframes containing hyperparameter values (glowca and sd15) to matrices
    mat_sd15 <- as.matrix(dat_sd15)
    mat_glo <- as.matrix(datglo)
    #compute cosine distance for each running pair of words in sd15 and glowca
    vals_sd15 <- unlist(lapply(2:nrow(mat_sd15), function(i){
      lsa::cosine(mat_sd15[i-1,],  mat_sd15[i,])
    }))
    vals_glo <- unlist(lapply(2:nrow(mat_glo), function(i){
      lsa::cosine(mat_glo[i-1,], mat_glo[i,])
    }))
    #Convert matrices back to dataframes
    message("So far so good... Now building output dataframe")
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
    mydists_unlemmatized %>%
      group_by(doc_id) %>%
      mutate(lemma2 = replace(lemma2, n(), NA)) %>%
      mutate(Sd15_Cosine = replace(Sd15_Cosine, n(), NA)) %>%
      mutate(Sd15_CosRev0Score = replace(Sd15_CosRev0Score, n(), NA)) %>%
      mutate(Glo_Cosine = replace(Glo_Cosine, n(), NA)) %>%
      mutate(Glowca_CosRev0Score = replace(Glowca_CosRev0Score, n(), NA)) %>%
      ungroup -> mydists_unlemmatized_NAatend
    #output a formatted dataframe
    return(as_tibble(mydists_unlemmatized_NAatend))
  }
}

