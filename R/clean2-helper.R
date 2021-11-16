clean2 <- function(x) {
  for (i in seq_along(replacements$target)) {
    y <- gsub(paste0('\\b',replacements$target[i],'\\b'), paste0(' ', replacements$replacement[i],' '), x)
  }
  return(y)
}
