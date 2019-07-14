#' Combine filters
#'
#'
#' @param chemtable the data frame of the data about the compounds
#' @keywords filters
#' @examples
#' data(GCMSfloral)
#' chemtable <- combine_filters(chemtable)
#' @export
combine_filters<-function(chemtable){
  num_filters <- sum(grepl("filter", names(chemtable)))

  chemtable$filters_passed <- rowSums(as.data.frame(lapply(data.frame(chemtable[, grepl("filter", names(chemtable))]=="OK"), as.integer)))

  chemtable$filter_final <- chemtable$filters_passed == num_filters

  return(chemtable)
}
