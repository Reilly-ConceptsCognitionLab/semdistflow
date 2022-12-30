#' Perform readtxt, cleanme, and distme in one step
#'
#' @name doitall
#' @export doitall

doitall <- function() {
  return(distme(cleanme(readme())))
}
