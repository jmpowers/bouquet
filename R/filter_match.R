#' Filter on the match between a compound and a library
#'
#' This function allows you to filter compounds that do not meet a minimum average match score with the library.
#' @param chemtable the data frame of the data about the compounds
#' @param min the minimum match score out of 1.
#' @keywords library match filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_match(chemtable, 0.7)
#' @export
filter_match<-function(chemtable, min=0.7){
  attr(chemtable, "match_min") <- min
  return(within(chemtable, {
    filter_match <- factor(ifelse(match >= min, "OK", "Low"))
  }))
}


