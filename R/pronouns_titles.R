#' Replace pronouns and titles with XX
#'
#' this code uses gsub to remove titles (Mr., Ms.) and pronouns (him, her) and replaces them with the XX (person, man, women).
#'
#' @param x A vector of words.

pronouns_titles <- function(x) {
  x <- gsub("(\\s+)(mr\\.)(\\s+)", " man ", x)
  x <- gsub("(\\s+)(mrs\\.)(\\s+)", " woman ", x)
  x <- gsub("(\\s+)(dr\\.)(\\s+)", " doctor ", x)
  x <- gsub("(\\s+)(ms\\.)(\\s+)", " woman", x)
  x <- gsub("\\,\\s{0,}(M|m)\\.(D|d)\\.", " doctor", x) #comma followed by 0 or more spaces m.d.
  x <- gsub("\\bhe\\b", "man", x)
  x <- gsub("\\bhim\\b", "man", x)
  x <- gsub("\\bshe\\b", "woman", x)
  x <- gsub("\\bher\\b", "woman", x)
  x <- gsub("\\bme\\b", "person", x)
  x <- gsub("\\bwe\\b", "people", x)
  x <- gsub("\\bus\\b", "people", x)
  x <- gsub("\\bour\\b", "people", x)
  x <- gsub("\\bthey\\b", "people", x)
  x <- gsub("\\bthem\\b", "people", x)
  x <- gsub("\\bthemselves\\b", "people", x)
  x <- gsub("\\bourselves\\b", "people", x)
  x <- gsub("\\btheirselves\\b", "person", x)
  x <- gsub("\\bhimself\\b", "man", x)
  x <- gsub("\\bherself\\b", "woman", x)
  x <- gsub("\\b(I|i)\\b", "person", x)
  x <- gsub("\\beverybody\\b", "people", x)
  x <- gsub("\\beveryone\\b", "people", x)
  x <- gsub("\\bherself\\b", "woman", x)
  x <- gsub("\\bhers\\b", "woman", x)
  x <- gsub("\\bmy\\b", "person", x)
  x <- gsub("\\bmyself\\b", "person", x)
  x <- gsub("\\bsomebody\\b", "person", x)
  x <- gsub("\\banybody\\b", "person", x)
  x <- gsub("\\banyone\\b", "person", x)
  x <- gsub("\\bhis\\b", "man", x)
  x <- gsub("\\bdoesn\\'t\\b", "", x)
  x <- gsub("\\betc\\.\\b", "", x)
  x <- gsub("\\bhadn\\'t\\b", " ", x)
}
