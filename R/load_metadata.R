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
#' metadata <- read.table("metadata.csv", header = TRUE, sep = "\t") %>%
#' load_metadata("SampleDate", "Sample", c("Species", "Population", "Drought"), "Type", "Biomass")
#' @export

load_metadata <- function(metadata, date, sample, group, type, amount) {
  colnames(metadata)[match(colnames(metadata), c(date, sample, group, type, amount))] <- c("date", "sample", "group", "type", "amount")
  return(metadata)
}
