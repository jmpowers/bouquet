#' Load metadata
#'
#' Loads metadata about the samples
#' @param metadata data frame of sample metadata
#' @param date the text date of sampling
#' @param sample the string that specifies the sample ID
#' @param group a vector of columns that specifies species or treatment groups
#' @param type the factor that specifies what type of sample it is (e.g. floral, ambient control, etc.). Levels should be c("floral", "ambient", "leaf")
#' @param amount the string that specifies the amount used for standardization (e.g. flower number or mass)
#' @return metadata, a data frame with the standard column names and types
#' @examples
#' data(GCMSfloral)
#' metadata <- load_metadata(GCMS_metadata, "SampleDate", "Filename", c("Cross", "Time"), "Type", "Flrs")
#' @export
load_metadata <- function(metadata, date=NULL, sample, group=NULL, type, amount=NULL) {
  cols <- as.list(environment())[c("date", "sample", "type", "amount")]
  cols <- cols[sapply(cols, is.character)]
  found <- match(colnames(metadata), cols)
  colnames(metadata) <- ifelse(is.na(found), colnames(metadata), names(cols)[found])

  attr(metadata,"group") <- as.vector(group)
  return(metadata)
}
