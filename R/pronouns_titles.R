#' Replace pronouns and titles with XX
#'
#' this code uses gsub to remove titles (Mr., Ms.) and pronouns (him, her) and replaces them with the XX (person, man, women).
#'
#' @param x A vector of words.
#' @keywords internal

pronouns_titles <- function(x) {
  x <- gsub("(\\s+)(mr\\.)(\\s+)", " man ", x)
  x <- gsub("(\\s+)(mrs\\.)(\\s+)", " woman ", x)
  x <- gsub("(\\s+)(dr\\.)(\\s+)", " doctor ", x)
  x <- gsub("(\\s+)(ms\\.)(\\s+)", " woman ", x)
  x <- gsub("\\,\\s{0,}(M|m)\\.(D|d)\\.", " doctor", x) #comma followed by 0 or more spaces m.d.
  x <- gsub("\\bdoesn\\'t\\b", "", x)
  x <- gsub("\\betc\\.\\b", "", x)
  x <- gsub("\\bhadn\\'t\\b", " ", x)
}

