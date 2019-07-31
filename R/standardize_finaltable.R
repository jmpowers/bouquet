#' Standardize finaltable by amount or row total
#'
#' Divides peak areas by the amount (biomass, counts) of each floral sample in metadata, or by the sample sum to create percentages.
#' @param finaltable the wide data frame with samples in rows and compound names in columns, containing peak areas for floral samples only
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param percent if TRUE, divide by row sums instead of amount
#' @return finaltable
#' @examples
#' data(GCMSfloral)
#' standardize_finaltable(finaltable, metadata, percent = FALSE)
#' @export
standardize_finaltable <- function(filetable, metadata, percent = FALSE) {
  return(finaltable / ifelse(percent, rowSums(finaltable), metadata[metadata$type == "floral","amount"])[row(finaltable)])
}
