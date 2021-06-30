#' Checks which elements of x are not found in y
#'
#' @param x first vector, to be checked
#' @param y second vector, checked against
#' @return BOOLean vector of elements of x not in y
#' @export
#' @examples
#' x <- c("A", "B", "C", "D")
#' y <- c("A", "B", "C")
#' 
#' x %nin% y
'%nin%' <- function(x, y) !('%in%'(x, y))
