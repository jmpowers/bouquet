#' Load GC-MS data
#'
#' Loads long-format GC-MS data and standardizes the column names and match scores
#' @param data the long format data frame
#' @param sample the string that species the sample ID
#' @param RT the string that specifies the peak area retention time
#' @param name the string that specifies the compound name
#' @param area the string that specifies the integrated peak area
#' @param match the numeric match score (dot product of unknown and database spectra)
#' @param maxmatch numerical value that specifies the maximum value possible for the match score (usually 100 or 1000)
#' @return longdata, a data frame with the standard column names and types, with match score relative to 1
#' @examples
#' read.table("GCMS_output.csv", header = TRUE, sep = "\t") %>%
#' load_longdata("Sample", "Time", "NIST_ID", "Area", "Score")
#' @export

load_longdata <- function(data, sample, RT, name, area, match, maxmatch = 100) {
  colnames(data)[match(colnames(data), c(sample, RT, name, area, match))] <- c("sample", "RT", "name", "area", "match")
  data$match <- data$match/maxmatch
  return(data)
}
