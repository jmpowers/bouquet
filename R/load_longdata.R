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
#' data(GCMSfloral)
#' longdata <- load_longdata(GCMS_output, "Sample", "RT", "Name", "Area", "Match", maxmatch=100)
#' @export
load_longdata <- function(data, sample, RT, name, area, match, maxmatch = 100) {
  cols <- as.list(environment())[c("sample", "RT", "name", "area", "match")]
  cols <- cols[sapply(cols, is.character)]
  found <- match(colnames(data), cols)
  colnames(data) <- ifelse(is.na(found), colnames(data), names(cols)[found])

  data$match <- data$match/maxmatch
  data$name <- as.factor(data$name)
  data$sample <- as.factor(data$sample)

  return(data)
}
