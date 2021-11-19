library(devtools)
library(roxygen2)
load_all()

doc_id <- "folder/test"
doc_text<- "Oldestone, the newest culinary interpretation of New Hope's iconic Old Stone Church, has opened at 15 S. Main Street. Next year, the internationally-recognized landmark will celebrate 150 years since its building.

The steak and seafood restaurant celebrates New American cuisine on its menu and salutes church history in its decor. Oldestone also features an authentic jazz and cocktail bar, open seven days a week.

The space was the former home of Marsha Brown's restaurant, which closed during the pandemic; Brown passed the baton to the new owners, including Wilfer Naranjo who had previously worked for her as a food runner. Oldestone will also feature several signature creole dishes from Marsha Brown's recipe collection."

testdata<-as.data.frame(cbind(doc_id,doc_text))

#working read in function
testfunction <- readin("/Users/bonniezuckerman/Desktop/texts")

#working clean data function
testdata.clean <- clean_df(testdata)

data("wiki_model")
data("semdist15")

#-------------- below codes are not ours!

#tokenize cleaned data (if not lemmatizing)
library(tidytext)
clean_tidy_text<- testdata.clean %>%
  unnest_tokens(word, doc_clean)
#lemmatize cleaned data
clean_tidy_text$lemma <- textstem::lemmatize_words(clean_tidy_text$word)

##-------------- below codes is working!
# joining
test <- rowwise_cosine_simil(data_file = clean_tidy_text, word_rating=wiki_model, colname1 = "word", colname2 = "Var1")
test.euc <- rowwise_euc_diff(data_file = test, word_rating=semdist15, colname1 = "word", colname2 = "word")

