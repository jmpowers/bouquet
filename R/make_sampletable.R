#' Make sample table
#'
#' Converts long-format GC-MS data to a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param longdata the long format data frame
#' @param metadata the sample metadata, rows will match order of metadata$sample
#' @return sampletable, a wide data frame with samples in rows and compound names in columns, containing peak areas
#' @examples
#' data(GCMSfloral)
#' sampletable <- make_sampletable(longdata, metadata)
#' @export
make_sampletable <- function(longdata, metadata) {
  sampletable <- reshape2::dcast(longdata, sample~name, sum, value.var="area")
  rownames(sampletable) <- sampletable$sample
  sampletable$sample <- NULL
  if(length(setdiff(as.character(metadata$sample), rownames(sampletable))) != 0 |
     length(setdiff(rownames(sampletable), as.character(metadata$sample))) != 0) {
    stop("Set of samples in longdata does not match set of samples in metadata")
  }
  sampletable <- sampletable[as.character(metadata$sample),]
  return(sampletable)
}
