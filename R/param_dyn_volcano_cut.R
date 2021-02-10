#' Used to parametrise the dyn_volcano_cut() function
#'
#' Can be used to parametrize the dyn_volcano_cut function if the standard parameters are not sufficient.
#' The hyperbolic function follows:
#'
#' y = s / (log2FC - log2FC.min) + FDR.min
#'
#' s ... "slope" of the function
#' log2FC ... log2 fold-change of features
#' log2FC.min ... minimum log2FC towards which the function converges towards in x: lim(f; y > 0) = log2FC.min
#' FDR.min ... minimum FDR the function converges to in y: lim(f; x > Inf) = FDR.min
#'
#' @param df a data.frame containing x and y coordinates for the two points the hyperbolic function is forces through
#' @param p_max maximal p.value/FDR allowed. Only relevant for very large fold-changes
#' @return list(s, FC_min) s ... "slope" of the hyperbolic function; FC_min ... minimum FC the hyperbolic function converges towards in x direction
#' @export
param_dyn_volcano_cut <- function(df = data.frame(x = c(1.5, 4),
                                                  y = c(0.001, 0.05)),
                                  p_max = 0.1) {
  # transform the minimal p-value into the volcano plot space
  p_max <- -log10(p_max)
  # and transform the set data points as well
  df[, "x"] <- log2(df[, "x"])
  df[, "y"] <- -log10(df[, "y"])

  # calculate the minimal FC for the given parameters
  # will be checked in dyn.volcano.cut for validity
  FC_min <-  (df[2, "x"]*df[2, "y"] -
                df[1, "x"]*df[1, "y"] +
                p_max*(df[1, "x"] - df[2, "x"])) /
    (df[2, "y"] - df[1, "y"])

  # calculate the slope/hyperbole of the function
  s <-  (df[1, "y"] - p_max) * (df[1, "x"] - FC_min)
  # s2 <- (df[2, "y"] - p_max) * (df[2, "x"] - FC_min) # debug to check if my assumptions were right

  return(list("FC_min" = 2^FC_min,
              "s" = s))
}
