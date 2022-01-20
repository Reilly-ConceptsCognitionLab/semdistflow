clean2_nopronouns <- function(x) {
  for (i in seq_along(replacements_nopronouns$target)) {
    y <- gsub(paste0('\\b',replacements_nopronouns$target[i],'\\b'), paste0(' ', replacements_nopronouns$replacement[i],' '), x)
  }
  return(y)
}
