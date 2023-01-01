#' Compute cosine distances anchored to first n lemmas
#'
#' Details here
#'
#' @name anchordist
#' @param targetdf a dataframe with long list of target words
#' @param anchorsize the size of the anchor block
#' @param windowsize the size of each window to compute cosine distances from anchor block
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
#' @export anchordist

#anchorsize is the number of words in the beginning of the language transcript upon which to base subsequent distance on
#window size is the number of words in a distance window (e.g., dog, leash, cat) averages vectors across words
anchordist <- function(targetdf, anchorsize, windowsize){
  message("Loading lookup databases and joining your data to SemDist15 and Glowca")
  #load lookup databases
  glowca_v1 <- readRDS(here("data", "glowca_vol1_2023.rda")) #rounded 5,subtlex matched 60k
  glowca_v2 <- readRDS(here("data", "glowca_vol2_2023.rda"))
  glowca <-  rbind(glowca_v1, glowca_v2)
  sd15 <-  readRDS(here("data", "semdist15_2023.rda"))
  #groups by factor variables and unlists the string, one word per row
  dat <- targetdf %>% group_by(doc_id, doc_text) %>% tidytext::unnest_tokens(word, doc_clean)
  #lemmatizes target dataframe on column labeled 'lemma1'
  dat2 <- dat %>% mutate(lemma1 = textstem::lemmatize_words(word))
  #join semdist15 and glowca lookup databases with target input dataframe
  joindf_semdist15 <- left_join(dat2, semdist15_new, by=c("lemma1"="word")) %>% data.frame()
  joindf_glowca <- left_join(dat2, glowca_lite, by=c("lemma1"="word")) %>% data.frame()
  # isolate rows the anchor words and their corresponding semantic vectors for SD15 based on user-defined anchor window
  anchor_semdist15 <- joindf_semdist15 %>% group_by(doc_id) %>% slice(1:anchorsize) # select rows corresponding to anchor size
  # isolate rows corresponding to the anchor words and their corresponding semantic vectors for glowca based on user-defined anchor window
  anchor_glowca <- joindf_glowca %>% group_by(doc_id) %>% slice(1:anchorsize)
  # compute mean semantic vector for the entire anchorword group (size specified in function)
  avganchor_semdist15 <- anchor_semdist15 %>% group_by(doc_id) %>% summarize(across(auditory:sadness, mean, na.rm = TRUE)) # mean vector of anchor
  # compute mean semantic vector for the entire anchorword group (size specified in function)
  avganchor_glowca <- anchor_glowca %>% group_by(doc_id) %>% summarize(across(X1:X300, mean, na.rm = TRUE))
  # give a window_index of 0 to combine with later window df
  avganchor_semdist15 <- avganchor_semdist15 %>% mutate(window_index = 0)
  avganchor_glowca <- avganchor_glowca %>% mutate(window_index = 0)
  # make window_index first column
  avganchor_semdist15 <- avganchor_semdist15 %>% select(c(doc_id, window_index), everything())
  avganchor_glowca <- avganchor_glowca %>% select(c(doc_id, window_index), everything())
  # select all rows not in anchor
  windows_semdist15 <- joindf_semdist15 %>% group_by(doc_id) %>% slice(anchorsize+1:n())
  windows_glowca <- joindf_glowca %>% group_by(doc_id) %>% slice(anchorsize+1:n())

  windows_semdist15 <- windows_semdist15 %>%
    group_by(doc_id) %>%
    mutate(window_index = (row_number() - 1) %/% windowsize) # give grouping # to each window, don't group across texts

  windows_glowca <- windows_glowca %>%
    group_by(doc_id) %>%
    mutate(window_index = (row_number() - 1) %/% windowsize)

  windows_summ_semdist15 <- windows_semdist15 %>%
    group_by(doc_id, window_index) %>%
    summarize(across(auditory:sadness, mean, na.rm = TRUE)) # decide if we want to remove NAs or not

  windows_summ_glowca <- windows_glowca %>%
    group_by(doc_id, window_index) %>%
    summarize(across(X1:X300, mean, na.rm = TRUE))

  # combine anchor and windows together into the same data frame
  anchor_window_df_semdist15 <- rbind(avganchor_semdist15, windows_summ_semdist15)
  anchor_window_df_glowca <- rbind(avganchor_glowca, windows_summ_glowca)

  # split data frame into list, with one data frame per doc_id
  semdist15_list <- split(anchor_window_df_semdist15, f = anchor_window_df_semdist15$doc_id)
  glowca_list <- split(anchor_window_df_glowca, f = anchor_window_df_glowca$doc_id)

  # remove the first two columns (not to be computed in cosine similarity)
  semdist15_df_list <- semdist15_list
  lapply(semdist15_df_list, function(x) x[!(names(x) %in% c("doc_id", "window_index"))]) -> semdist15_df_list

  glowca_df_list <- glowca_list
  lapply(glowca_df_list, function(x) x[!(names(x) %in% c("doc_id", "window_index"))]) -> glowca_df_list

  # compute cosine of each row relative to first row
  sapply(semdist15_df_list, function(x) mapply(function(u, v)
    c(lsa::cosine(as.vector(u), as.vector(v))),
    asplit(x[1, ], 1), asplit(x[1:nrow(x), ], 1))) -> semdist15_cos

  sapply(glowca_df_list, function(x) mapply(function(u, v)
    c(lsa::cosine(as.vector(u), as.vector(v))),
    asplit(x[1, ], 1), asplit(x[1:nrow(x), ], 1))) -> glowca_cos

  # convert lists to data frames
  vals_sd15 <- as.data.frame(semdist15_cos)
  vals_glo <- as.data.frame(glowca_cos)

  # rename column of cosine distance values relative to first row
  names(vals_sd15)[1] <- "Sd15_Cosine_RelRow1"
  names(vals_glo)[1] <- "Glo_Cosine_RelRow1"

  # reverse scale the cosine values for glowca and sd15, subtract each obs from max (1)
  sd15vals <- vals_sd15 %>% mutate(Sd15_CosRev0Score_RelRow1= 1-Sd15_Cosine_RelRow1)
  glowcavals <- vals_glo %>% mutate(Glowca_CosRev0Score_RelRow1 = 1-Glo_Cosine_RelRow1)

  ## rebuild dataframe
  # pull out words in anchor
  anchor_words_sd15 <- as.data.frame(anchor_semdist15$lemma1)
  names(anchor_words_sd15)[1] <- "lemmas"
  grouped_anchor <- rbind(anchor_words_sd15, grouped = apply(anchor_words_sd15, 2, paste0, collapse = " "))
  grouped_anchor <- tail(grouped_anchor, n=1)

  # pull out words in windows
  windows_words_sd15 <- as.data.frame(windows_semdist15$lemma1)
  names(windows_words_sd15)[1] <- "lemmas"

  # add NAs if necessary so number of rows is multiple of window size
  rem <- nrow(windows_words_sd15) %% windowsize # count remainder
  while (rem != 0){
    windows_words_sd15[nrow(windows_words_sd15) + 1, ] <- NA
    rem <- nrow(windows_words_sd15) %% windowsize # recount remainder
  }

  grouped_window <- data.frame(matrix(windows_words_sd15$lemmas, ncol = windowsize, byrow = TRUE))
  cols <- colnames(grouped_window)
  grouped_window$lemmas <- do.call(paste, c(grouped_window[cols], sep = " ")) # concatenate all columns to get all words in each window
  grouped_window <- subset(grouped_window, select = "lemmas")

  # combine anchor and window words together
  grouped_words <- rbind(grouped_anchor, grouped_window)

  # bind the cosine data to the anchor/window dataframe
  anchorwindow_dists <- cbind(grouped_words, sd15vals, glowcavals)
  rownames(anchorwindow_dists) <- 1:nrow(anchorwindow_dists) # renumber rows (got funky with anchoring/windowing)
  return(anchorwindow_dists)
  }

