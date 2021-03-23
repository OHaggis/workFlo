#' Returns membership information on intersections of a VennDiagram
#'
#' Best used together with the plotVenn() function.
#'
#' @param x A list of (named) character vectors indicating the different groups.
#' @import dplyr
#' @return a tibble containing BOOLean columns
#' @export
#' @examples
#' # define a (named) list containing three named character vectors
#' list_4Venn <- list(GroupA = c("Hans", "Jane", "Mark", "Gudrun"),
#'                    GroupB = c("Jane", "Gudrun", "Simon", "Flo"),
#'                    GroupC = c("Simon", "Jane", "Theordore", "Jessica"))
#'
#' plotVenn(list_4Venn)
#' 
#' # get membership information
#' get_intersect_members(list_4Venn)
get_intersect_members <- function(x = list()) {
  if(data.class(x) != "list") stop("Function works on list elements")
  
  if(!all(lapply(x, function(y) data.class(y) == "character"))) return("non-character included")
  # build vector of all elements of the vectors in x
  print("data is list of characters")
  
  id <- Reduce(union, x)
  col_names <- names(x)
  
  # check whether the ids are found in the elements of x
  group_aff <- lapply(x, function(x) id %in% x)
  
  # make a tibble
  tb <- tibble(name = id) %>% 
    bind_cols(group_aff)
  
  return(tb)
}
