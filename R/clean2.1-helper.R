clean2.1 <- function(x) {
  y <- strsplit(x," ")
  z <- stringi::stri_replace_all_regex(x, "^"%s+%replacements$target%s+%"$", replacements$replacement, vectorize_all = TRUE)
  return(z)
}
