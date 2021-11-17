clean3 <- function(x) {
  x <- gsub("person person", "person", x)
  x <- gsub("people people", "people", x)
  x <- gsub("woman person", "woman", x)
  x <- gsub("man person", "man", x)
  x <- gsub("person people", "person", x)
  x <- gsub("man man", "man", x)
  x <- gsub("woman woman", "woman", x)
  x <- gsub("(.*)(, and* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(, but* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(,* because* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub("(.*)(,* then* )(\\w{1,}\\s\\w{1,})", "\\1. \\3", x)
  x <- gsub(",", "", x) # remove commas
  x <- gsub("([[:punct:]])", "", x) # remove periods
  #x <- gsub("([[:alpha:]]) ([[:punct:]])", "\\1\\2", x) #omit space between any character and a period
  #x <- gsub("\\s([[:punct:]])", "", x) #omits floating periods from utterance void of content words
  #x <- gsub("\\.{2,}", "\\. ", x)
  x <- tm::stripWhitespace(x)
  #x <- stringi::stri_remove_empty(x)
}
