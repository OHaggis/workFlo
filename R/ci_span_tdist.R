#' Calculates the margin of error of a numeric vector from a t-distribution
#'
#' Is used to calculate confidence intervals from t-distributions: mean +/- margin of error.
#' Has a partner function using normal-distributions: ci_span_norm
#'
#' @import ggplot2
#' @import dplyr
#' @param y a numeric vector
#' @return the margin of error
#' @export
#' @examples
#' set.seed(42)
#'
#' y <- runif(100, 1, 100)
#'
#' # the margin of error
#' ci_span_tdist(y)
#'
#' # lower end of CI
#' mean(y) - ci_span_tdist(y)
#'
#' # upper end of CI
#' mean(y) + ci_span_tdist(y)
#'
#' # use in ggplots
#' tibble(x = sample(c("a", "b"), 100, replace = TRUE),
#'        y = y) %>%
#'   ggplot(aes(x = x,
#'              y = y)) +
#'   geom_point(alpha = 0.5,
#'              position = position_jitter(width = 0.1)) +
#'   stat_summary(geom = "errorbar",
#'                fun.min = function(v) mean(v) - ci_span_tdist(v),
#'                fun.max = function(v) mean(v) + ci_span_tdist(v),
#'                col = "red") +
#'   theme_classic()
ci_span_tdist <- function(y) qt(0.975, df = length(!is.na(y)) - 1) * sd(y, na.rm = T)/sqrt(length(!is.na(y)))
