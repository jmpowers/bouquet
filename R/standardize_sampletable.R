#' Standardize sampletable by amount or row total
#'
#' Divides peak areas by the amount (biomass, counts) of each sample in metadata, or by the sample sum to create percentages.
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param percent if TRUE, divide by row sums instead of amount
#' @return sampletable
#' @examples
#' data(GCMSfloral)
#' standardize_sampletable(sampletable, metadata, percent = FALSE)
#' @export
standardize_sampletable <- function(sampletable, metadata, percent = FALSE) {
  return(sampletable / ifelse(percent, rowSums(sampletable), metadata$amount)[row(sampletable)])
}
