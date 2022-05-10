#' Filter on the maximum peak area of a compound
#'
#' This function allows you to filter compounds that only appear in very small amounts.
#' @param chemtable the data frame of the data about the compounds
#' @param min_maximum the minimum value for maximum peak area of a compound
#' @keywords area filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_area(chemtable, min_maximum = 20000)
#' @export
filter_area<-function(chemtable, min_maximum){
  attr(chemtable, "area_min_maximum") <- min_maximum
  return(within(chemtable, {
    filter_area <- factor(ifelse(max.floral > min_maximum, "OK", "Low"))
  }))
}
