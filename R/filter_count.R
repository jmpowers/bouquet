#' Filter on compound counts
#'
#' This function allows you to filter compounds are rare in the entire dataset or within certain groups.
#' @param chemtable the data frame of the data about the compounds
#' @param min_count the minimum count value for inclusion
#' @param group boolean, whether to set thresholds within each grouping level, or look across all floral samples
#' @keywords count filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_count(chemtable, min_count = 3, group = TRUE)
#' @export
filter_count<-function(chemtable, min_count = 2 , group = FALSE){
  attr(chemtable, "count_min") <- min_count
  cols <- "count.floral"
  if(group) cols <- colnames(chemtable)[grepl("^count.", colnames(chemtable)) &
                                          !(colnames(chemtable) %in% c("count.ambient","count.blank"))]
  for(col in cols) {
    chemtable[,paste0("filter_",col)] <- factor(ifelse(chemtable[,col] >= min_count, "OK", "Rare"))
  }
  return(chemtable)
}
