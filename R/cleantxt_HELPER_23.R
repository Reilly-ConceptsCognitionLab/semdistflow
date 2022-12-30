cleantxt<- function(x) {
  omissions <- load(here::here("data", "omissions_2023.rda"))
  replacements <- load(here::here("data", "replacements_2023.rda"))
  y <- x
  x <- x$doc_text
  x <- tolower(x) #to lower
  x <- gsub("\"", " ", x)
  x <- gsub("\n", " ", x)
  x <- textclean::replace_date(x)
  x <- stringi::stri_replace_all_regex(x, "^" %s+% replacements$target %s+% "$", replacements$replacement, vectorize_all = FALSE)
  x <- gsub("`", "'", x)  # replaces tick marks with apostrophe for contractions
  x <- gsub("(\\d)(\\.)", "", x)   #look for period adjacent to any digit, replace with nothing
  x <- textclean::replace_contraction(x) #replace contractions
  x <- gsub("([[:alpha:]])([[:punct:]])", "\\1 \\2", x) #add a space between any alphabetic character and punctuation
  x <- gsub("-", " ", x) #replace all hyphens with spaces
  x <- tm::removeWords(x, omissions$target)
  x <- gsub("\\d+(st|nd|rd|th)", " ", x) #omits 6th, 23rd, ordinal numbers
  x <- gsub("[^a-zA-Z;.,]", " ", x) #omit numbers and most punctuation, retain alphabetic chars, comma, semicolon, periods
  x <- gsub("\\b[a-z]\\b{1}", " ", x) #omits any singleton alphabetic character
  x <- gsub("\\;", "\\.", x) #replace semicolons with periods
  x <- gsub("\\s{2,}", " ", x) #replace two or more spaces with a single space
  x <- unlist(strsplit(x, " "))
  x <- paste(x,collapse=" ")
  x <- gsub("person person", "person", x)
  x <- gsub("people people", "people", x)
  x <- gsub("(.*)(, and* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(, but* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(,* because* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(,* then* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub(",", "", x) # remove commas
  x <- gsub("([[:punct:]])", "", x) # remove periods
  doc_clean <- tm::stripWhitespace(x)
  cleandoc <- cbind(y, doc_clean)
  return(cleandoc)
}

