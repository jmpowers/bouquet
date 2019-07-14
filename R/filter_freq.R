#' Filter on compound frequency
#'
#' This function allows you to filter compounds are rare in the entire dataset or within certain groups.
#' @param chemtable the data frame of the data about the compounds
#' @param min_freq the minimum frequency value for inclusion
#' @param group boolean, whether to set thresholds within each grouping level, or look across all floral samples
#' @keywords frequency filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_freq(chemtable, min_count = 0.2, group = TRUE)
#' @export
filter_freq<-function(chemtable, min_freq = 0.1, group = FALSE){
  attr(chemtable, "freq_min") <- min_freq
  cols <- "freq.floral"
  if(group) cols <- colnames(chemtable)[grepl("^freq.", colnames(chemtable)) &
                                          !(colnames(chemtable) %in% c("freq.ambient","freq.blank"))]
  for(col in cols) {
    chemtable[,paste0("filter_",col)] <- factor(ifelse(chemtable[,col] >= min_freq, "OK", "Rare"))
  }
  return(chemtable)
}
