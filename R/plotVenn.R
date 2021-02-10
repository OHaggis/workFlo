#' Plots Venn Diagram
#'
#' Plot Venn-diagrams from a list of named character vectors.
#' This is used to indicate the overlaps in different groups.
#' Beaware: Accurate solutions are not possible for comparisons of many groups. More than 5 groups is generally not advised.
#'
#' @param x A list of (named) character vectors indicating the different groups.
#' @param shape Either "ellipse" or "circle". Ellipses are more accurate especially in cases of more than three groups.
#' @importFrom eulerr euler
#' @return no return value
#' @export
#' @examples
#' # define a (named) list containing three named character vectors
#' list_4Venn <- list(GroupA = c("Hans", "Jane", "Mark", "Gudrun"),
#'                    GroupB = c("Jane", "Gudrun", "Simon", "Flo"),
#'                    GroupC = c("Simon", "Jane", "Theordore", "Jessica"))
#'
#' plotVenn(list_4Venn)
plotVenn <- function(x = list(),
                     shape = "ellipse") {
  if(data.class(x) == "list") {

    if(!all(lapply(x, function(y) data.class(y) == "character"))) return("non-character included")
    # build vector of all elements of the vectors in x
    print("data is list of characters")

    id <- Reduce(union, x)
    col_names <- names(x)

    # check whether the ids are found in the elements of x
    group_aff <- lapply(x, function(x) id %in% x)

    # cbind all the elements (now boolean)
    group_aff <- Reduce(cbind, group_aff)
    colnames(group_aff) <- col_names

    venn_map <- euler(group_aff,
                      shape = shape)
  }
  if(data.class(x) == "matrix") {
    print("data is matrix")

    venn_map <- euler(x,
                      shape = shape)
  }
  plot(venn_map,
       quantities = TRUE)
}
