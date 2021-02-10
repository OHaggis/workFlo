#' Transforms a tibble into a matrix
#'
#' This function was excluded from dplyr, but is still usefull in many workflows.
#' It takes a tibble and transforms it into a matrix. Rownames are taken from the first column, which is subsequently deleted. Colnames are kept.
#' Columns should be selected before transformation.
#'
#' @import dplyr
#' @import ggplot2
#' @param tb a tibble
#' @param long is the source in long format? Restructures into wide format. Use with caution.
#' @param out_type c("numeric", "character"); preferred output data.class. Will introduce NAs from characters if forced to "numeric".
#' @return no return value
#' @export
#' @examples
#' # wide format data
#' tb_wide <- as_tibble(mtcars,
#'                      rownames = "model")
#'
#' tb_wide %>%
#'   as_matrix()
#' tb_wide %>%
#'   as_matrix(out_type = "character")
#'
#' # from long format, not recommended
#' tb_long <- as_tibble(mtcars,
#'                      rownames = "model") %>%
#'   pivot_longer(2:12)
#'
#' tb_long %>%
#'   as_matrix(long = TRUE)
as_matrix <- function(tb,
                      long = FALSE,
                      out_type = "numeric") {
  if(long == TRUE) {
    tb <-
      tb %>%
      pivot_wider(names_from = 2,
                  values_from = 3)
  }

  m_rownames <- tb[, 1] %>% pull

  tb <- tb[, -1]

  m <- as.matrix(tb)

  if(out_type == "numeric") m <- apply(m, 2, as.numeric)
  if(out_type == "character") m <- apply(m, 2, as.character)

  rownames(m) <- m_rownames

  return(m)
}
