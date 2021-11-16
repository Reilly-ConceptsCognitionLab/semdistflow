clean1 <- function(x) {
  x <- tolower(x) #to lower
  x <- gsub("\"", " ", x)
  x <- gsub("\n", " ", x)
  x <- replace_date(x)
  x <- gsub("`", "'", x)  # replaces tick marks with apostrophe for contractions
  x <- gsub("(\\d)(\\.)", "", x)   #look for period adjacent to any digit, replace with nothing
  x <- pronouns_titles(x)
  x <- replace_contraction(x) #replace contractions
  x <- gsub("([[:alpha:]])([[:punct:]])", "\\1 \\2", x) #add a space between any alphabetic character and punctuation
  x <- gsub("I", "i", x)
  x <- gsub("A", "a", x)
  x <- gsub("-", " ", x) #replace all hyphens with spaces
  x <- removeWords(x, omissions$target)
  x <- gsub("\\d+(st|nd|rd|th)", " ", x) #omits 6th, 23rd, ordinal numbers
  x <- gsub("[^a-zA-Z;.,]", " ", x) #omit numbers and most punctuation, retain alphabetic chars, comma, semicolon, periods
  x <- gsub("\\b[a-z]\\b{1}", " ", x) #omits any singleton alphabetic character
  x <- gsub("\\;", "\\.", x) #replace semicolons with periods
  x <- gsub("\\s{2,}", " ", x) #replace two or more spaces with a single space
}
