#' Checks which elements of x are not found in ydevtool
#'
#' @param x first vector, to be checked
#' @param y second vector, checked against
#' @return BOOLean vector of elements of x not in y
#' @export
#' @examples
#' x <- c("A", "B", "C", "D")
#' y <- c("A", "B", "C")
#' 
#' x %!in% y
'%!in%' <- function(x, y) !('%in%'(x, y))
