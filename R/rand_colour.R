#' Returns n random colours from all RColorBrewer palettes
#' 
#' This was adapted from:
#' https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
#' User: JelenaCuklina
#' 
#' Max. retrieved different colours: 74
#' 
#' @import RColorBrewer
#' @export
#' @param n number of colours to return
#' @examples 
#' rand_colour(100)
rand_colour <- function(n = 20) {
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector <-  unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  return(sample(col_vector, n))
}