#' Opens a ggplot2 plot in an external window
#'
#' If R struggles with plotting large and complex plots, this function
#' exports the plot to a temporary file and opens it in an external
#' window.
#' This bypasses the sometimes rather slow plotting by R.
#'
#' @import PBSmodelling
#' @import ggplot2
#' @param plot A ggplot2 object
#' @return no return value
#' @export
#' @examples
#' # plot the displacement of car eniges over the number of cylinders
#' p <-
#'   mpg %>%
#'     ggplot(aes(x = cyl,
#'                y = displ)) +
#'   geom_bar(stat = "identity") +
#'   theme_classic()
#'
#' # export to temporary file and open
#' open_png(p)
open_png <- function(plot,
                     width = 1000,
                     height = 700,
                     legend.title.size = 15,
                     legend.text.size = 12) {
  i <- 1
  filename <- paste(tempdir(),
                    format(Sys.Date(), "%g%m%d"),
                    "_temp_",
                    i,
                    ".png",
                    sep = "")

  while(file.exists(filename)) {
    i <-  i + 1
    filename <- paste(tempdir(),
                      format(Sys.Date(), "%g%m%d"),
                      "_temp_",
                      i,
                      ".png",
                      sep = "")
  }

  png(filename = filename,
      width = width,
      height = 700)
  print(plot +
          theme(legend.title = element_text(size = 15),
                legend.text = element_text(size = 12)))
  dev.off()

  openFile(filename)
}
