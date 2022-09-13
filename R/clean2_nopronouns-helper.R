clean2_nopronouns <- function(x) {
  y <- unlist(strsplit(x, " "))
  z <- stringi::stri_replace_all_regex(y, "^" %s+% db_replacements_nopronouns$target %s+% "$", db_replacements_nopronouns$replacement, vectorize_all = FALSE)
  a <- paste(z,collapse=" ")
  return(a)
}
