#' Make sample table
#'
#' Converts long-format GC-MS data to a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param longdata the long format data frame
#' @return sampletable, a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @examples
#' data(GCMSfloral)
#' sampletable <- make_sampletable(longdata)
#' @export
make_sampletable <- function(longdata) {
  sampletable <- reshape2::dcast(longdata, sample~name, sum, value.var="area")
  rownames(sampletable) <- sampletable[,"sample"]
  sampletable[,"sample"] <- NULL
  return(sampletable)
}
