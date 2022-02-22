library(devtools)
library(roxygen2)
library(tidyverse)
library(stringi)
load_all()
document()


library(dplyr)
#working read in function
testfunction_multi <- readin("/Users/bonniezuckerman/Desktop/ReillyLab/multi_texts/short")
outputtest3 <-clean_df_bygroup(testfunction_multi)
data("wiki_model")
data("semdist15")

#-------------- below codes are not ours!

#tokenize cleaned data (if not lemmatizing)
library(tidytext)
library(tidyverse)
clean_tidy_text<- outputtest3 %>%
  group_by(doc_id, doc_text) %>%
  unnest_tokens(word, doc_clean)
#lemmatize cleaned data
clean_tidy_text$lemma<- textstem::lemmatize_words(clean_tidy_text$word)

##-------------- below codes is working!
# joining

test <- rowwise_cosine_simil(targetdf = clean_tidy_text, lookupdb = wiki_model, colname1 = lemma, colname2 = Var1)
test %>% group_by(doc_id) %>% slice(head(row_number(),5))
test.sem <- rowwise_cosine_simil(targetdf = clean_tidy_text, lookupdb=semdist15, colname1 = lemma, colname2 = word)
test.sem %>% group_by(doc_id) %>% slice(head(row_number(),5))
sticks_cosine<- left_join(test, test.sem, by = c("doc_id", "doc_text", "word", "lemma_pair1", "lemma"), suffix=c(".glove",".semdist15"))
sticks_cosine %>% group_by(doc_id) %>% slice(head(row_number(),5))
write.csv(sticks_cosine, file = "sticks_cosine_213.csv")

