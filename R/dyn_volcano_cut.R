#' Used to calculate a dynamic cut of for volcano-plots
#'
#' This function calculates whether a data point defined ny p-values or FDRs and fold-change lies outside of a certain parabolic cut-off in a volcano plot.
#' Conventionally, cut-offs are often set static, defining a rectangle in both positive and negative fold-change space indicating relevant features.
#' Using a dynamic cut-off (which is always stricter than the static cut-off) one can reduce the number of signidicant features, while
#' including not significant features (max. p-value/FDR = 0.1) with large fold-changes that could be biologically interesting.
#'
#' Parameters ("param") can be re-parametrized using param_dyn_volcano_cut, also available in this package.
#' These are parameters for the hyperbolic curve, forcing it through 2 points on the grid in positive direction, mirrored to the negative fold-changes.
#' Standard parameters force the cut-off through A = c(FC = 1.5, p-value = 0.001) and B = c(FC = 4, 0.05).
#' By doing so, it allows for potentially significant results with i) fold-changes below 50% but high significance below p < 0.001,
#' and ii) fold-changes above 400% and p-values slightly above 0.05.
#'
#' @import ggplot2
#' @import dplyr
#' @param FC fold-change of features in a volcano analysis
#' @param p_max maximal accepted p-value for cut-off. Only relevant for very high fold-changes.
#' @param param parameters setting the hyperbolic curve shape. Can be paramterized by param_dyn_volcano_cut
#' @return a BOOLean vector indicating wheter a feature lies outside of the cut-off, therefore marking it as relevant
#' @export
#' @examples
#' data("volcano_analysis")
#'
#' # calculate the needed FDR cut-off for a given log2-FC
#' # test if testd FDR lies above cut-off
#' volcano_analysis2 <- volcano_analysis %>%
#'   mutate(FDR_cutoff = dyn_volcano_cut(log2FC),
#'          above_cutoff = FDR < FDR_cutoff)
#'
#' # plot an explanation
#' volcano_analysis2 %>%
#'   ggplot(aes(x = log2FC,
#'              y = -log10(FDR))) +
#'   geom_point(aes(col = above_cutoff)) +
#'   stat_function(fun = function(x) -log10(dyn_volcano_cut(FC = x)),
#'                 n = 1001,
#'                 colour = "green") +
#'   # first "forced" point of hyperbole, parametrised
#'   # standard parameter
#'   annotate(geom = "point",
#'            x = log2(1.5),
#'            y = -log10(0.001),
#'            pch = 4,
#'            size = 5,
#'            col = "red") +
#'   annotate(geom = "text",
#'            x = log2(1.5) - 0.25,
#'            y = -log10(0.0008),
#'            col = "red",
#'            label = "point 1") +
#'   # second "forced" point of hyperbole, parametrised
#'   # standard parameters
#'   annotate(geom = "point",
#'            x = log2(4),
#'            y = -log10(0.05),
#'            pch = 4,
#'            size = 5,
#'            col = "red") +
#'   annotate(geom = "text",
#'            x = log2(4),
#'            y = -log10(0.1),
#'            col = "red",
#'            label = "point 2") +
#'   coord_cartesian(ylim = c(0, max(-log10(volcano_analysis2$FDR)))) +
#'   theme_classic()
dyn_volcano_cut <- function(FC, p_max = 0.1, param = list("s" = 0.5014435, "FC_min" = 1.260714)) {

  if(((param$s / ((-0.0001) - log2(param$FC_min))) + -log10(p_max)) >= 0) {
    print("Warning: FC_min parametrized too low!")
  }

  cut <- ifelse(FC < log2(1/param$FC_min),
                (param$s / ((-FC) - log2(param$FC_min))) + -log10(p_max),
                ifelse(FC > log2(param$FC_min),
                       (param$s / ((FC) - log2(param$FC_min))) + -log10(p_max),
                       Inf))

  return(10^-cut)
}
