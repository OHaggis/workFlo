#' Returns the factor levels of a column
#' 
#' Can be used to get an idea which unique elements a vector contains.
#' Does not need to be a factor vector.
#' 
#' @import dplyr
#' @import ggplot2
#' @export
#' @param tb a tibble containing the column of interest
#' @param column name of the column
#' @example 
#' mpg %>% 
#'   lvl(cyl)
lvl <- 
  function(tb, column) {
    lvls <- 
      pull(tb, {{ column }}) %>% 
      unique
    
    return(lvls)
}