library(devtools)
library(roxygen2)
load_all()

wiki_model_50 <- read.csv("/Users/bonniezuckerman/Documents/GitHub/AphasiaBank/text_tools/wiki_model.csv", header = T)
wiki_model_100 <- read.csv("/Users/bonniezuckerman/Documents/GitHub/AphasiaBank/text_tools/wiki_model_100d.csv", header = T)
replacements_nopronouns <- read.csv("/Users/bonniezuckerman/Desktop/SticksandPoem/db_replacements_pronounsremoved.csv", header = T)
usethis::use_data(replacements_nopronouns)
usethis::use_data(wiki_model_50)
usethis::use_data(wiki_model_100)

rm("wiki_model_300.rda")

paths <- sort(Sys.glob(c("data/*.rda", "data/*.RData")))
res <- tools::checkRdaFiles(paths)

document()

doc_id <- "folder/test"
doc_text<- "Oldestone, the newest culinary interpretation of New Hope's iconic Old Stone Church, has opened at 15 S. Main Street. Next year, the internationally-recognized landmark will celebrate 150 years since its building.

The steak and seafood restaurant celebrates New American cuisine on its menu and salutes church history in its decor. Oldestone also features an authentic jazz and cocktail bar, open seven days a week.

The space was the former home of Marsha Brown's restaurant, which closed during the pandemic; Brown passed the baton to the new owners, including Wilfer Naranjo who had previously worked for her as a food runner. Oldestone will also feature several signature creole dishes from Marsha Brown's recipe collection."

testdata<-as.data.frame(cbind(doc_id,doc_text))

#working read in function
testfunction <- readin("/Users/bonniezuckerman/Desktop/texts")
testfunction_multi <- readin("/Users/bonniezuckerman/Desktop/multi_texts/short")


#working clean data function
testdata.clean <- clean_df(testfunction)

data("wiki_model")
data("semdist15")

#-------------- below codes are not ours!

#tokenize cleaned data (if not lemmatizing)
library(tidytext)
library(tidyverse)
clean_tidy_text<- testdata.clean %>%
  unnest_tokens(word, doc_clean)
#lemmatize cleaned data
clean_tidy_text$lemma<- textstem::lemmatize_words(clean_tidy_text$word)

##-------------- below codes is working!
# joining

test <- rowwise_cosine_simil(targetdf = clean_tidy_text, lookupdb = wiki_model, colname1 = lemma, colname2 = Var1)
test %>% group_by(doc_id) %>% slice(head(row_number(),5))

test.sem <- rowwise_cosine_simil(targetdf = clean_tidy_text, lookupdb=semdist15, colname1 = lemma, colname2 = word)
test.euc %>% group_by(doc_id) %>% slice(head(row_number(),5))
sticks_cosine<- left_join(test, test.sem, by = c("doc_id", "doc_text", "word", "lemma_pair1", "lemma"), suffix=c(".glove",".semdist15"))

sticks_cosine<- merge(test, test.sem, by = 0:5, suffix=c(".glove",".semdist15"), sort=F)
write.csv(sticks_cosine, file = "sticks_cosine_213.csv")

library()
browseVignettes("gazer")
