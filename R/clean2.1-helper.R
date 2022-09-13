clean2.1 <- function(x) {
  y <- unlist(strsplit(x, " "))
  z <- stringi::stri_replace_all_regex(y, "^" %s+% db_replacements$target %s+% "$", db_replacements$replacement, vectorize_all = FALSE)
  a <- paste(z,collapse=" ")
  return(a)
}
