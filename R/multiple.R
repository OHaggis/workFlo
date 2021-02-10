#' Checks if elements are duplicated
#'
#' @param v a vector
#' @return a BOOLean vector indicating all elements with duplicates in v
#' @export
#' @examples
#' v <- c("a", "b", "c", "a", "d")
#' multiple(v)
multiple <- function(v) v %in% unique(v[duplicated(v)])
