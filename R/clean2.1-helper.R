clean2.1 <- function(x) {
    y <- stringi::stri_replace_all_regex(x, "^"%s+%replacements$target%s+%"$", replacements$replacement, vectorize_all = FALSE)
  return(y)
}
