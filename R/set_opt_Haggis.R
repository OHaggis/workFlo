#' Setting some global options
#' 
#' 
#' 
#' @export
#' 
set_opt_Haggis <- function() {
  # global R options ----
  options(stringsAsFactors = FALSE,
          scipen = 9999)
  options(repos = getOption("repos")["CRAN"])
  
  Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
  
  
  # ggplot options ----
  theme_update(axis.title = element_text(size = 8, colour = "black"),
               axis.text = element_text(size = 6, colour = "black"),
               legend.title = element_text(size = 8, colour = "black"),
               legend.text = element_text(size = 6, colour = "black"),
               strip.text = element_text(size = 8, colour = "black"),
               axis.ticks = element_line(colour = "black"))
  theme_set(theme_minimal() +
              theme(axis.text = element_text(size = 6,
                                             colour = "black"),
                    axis.line = element_line(colour = "black"),
                    axis.title = element_text(size = 8)))
}