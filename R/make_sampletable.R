#' Make sample table
#'
#' Converts long-format GC-MS data to a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param longdata the long format data frame
#' @return sampletable, a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @examples
#' sampletable <- make_sampletable(longdata)
#' @export
#'
make_sampletable <- function(longdata) {
  library(reshape2)
  sampletable <- dcast(longdata, sample~name, sum, value.var="area")
  rownames(sampletable) <- sampletable[,1]
  sampletable[,1] <- NULL
  return(sampletable)
}
