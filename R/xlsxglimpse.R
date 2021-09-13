#' Opens a tibble or data.frame in an Excel Window
#'
#' The main purpose of this function is to display data structures in Excel
#' OR
#' export data for quick saving.
#'
#' This function exports a tibble or data.frame to the temporary folder of R
#' and opens it in an excel-file.
#' The temporary file folder of R is erased with every clean shut-down, don't forget
#' to save crucial data.
#' Also be aware that Excel has problems properly loading and viewing large files.
#' Use matrix operations or tidy expressions to subset large data sets before exporting to Excel.
#'
#' @param df|tb, data.frame or tibble to be displayed
#' @import openxlsx
#' @import dplyr
#' @return no return value
#' @export
#' @examples
#' # open the "mpg" data set in an Excel window
#' library(ggplot2) # contains the mpg data set
#' print(mpg)
#'
#' # direct call
#' xlsxglimpse(mpg)
#'
#' # tidy implementation
#' mpg %>%
#'   select(manufacturer, model, cyl, class) %>%
#'   filter(cyl > 4) %>%
#'   xlsxglimpse()
xlsxglimpse <- function(df) {
  
  if(is.matrix(df)) df <- as_tibble(df, rownames = "rownames")
  
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "sheet1")

  openxlsx::writeData(wb, "sheet1", df)

  openxlsx::freezePane(wb, "sheet1", firstRow = TRUE)

  headerStyle <- openxlsx::createStyle(fgFill = "#ccffdd", border="Bottom", borderColour = "#000000",
                             textDecoration = "bold")

  openxlsx::addStyle(wb, "sheet1", headerStyle, rows = 1, cols = 1:dim(df)[2])

  openxlsx::addFilter(wb, "sheet1", rows = 1, cols = 1:dim(df)[2])

  openxlsx::setColWidths(wb, "sheet1", cols = 1:dim(df)[2], widths = 15)

  i <- 1
  filename <- paste(tempdir(),
                    "\\",
                    format(Sys.Date(), "%g%m%d"),
                    "_temp_",
                    i,
                    ".xlsx",
                    sep = "")

  while(file.exists(filename)) {
    i <-  i + 1
    filename <- paste(tempdir(),
                      "\\",
                      format(Sys.Date(), "%g%m%d"),
                      "_temp_",
                      i,
                      ".xlsx",
                      sep = "")
  }

  openxlsx::saveWorkbook(wb,
                         file = filename)
  
  # PBSmodelling::openFile(filename)
  
  open_command <- switch(Sys.info()[['sysname']],
                         Windows= 'open',
                         Linux = 'xdg-open',
                         Darwin = 'open')
  system(paste(open_command, filename))

}
