#' Prune sampletable by date
#' Zeros out cells in the sample table that did not pass the ambient date filter.
#'
#' @param chemtable the data frame of the data about the compounds
#' @keywords filters
#' @examples
#' data(GCMSfloral)
#' chemtable <- prune_sampletable_dates(ampletable, chemtable, metadata, datefilter)
#' @export
prune_sampletable_dates<-function(sampletable, chemtable, metadata, datefilter){
  for(i in colnames(datefilter)) {
    sampletable[metadata$type == "floral" & metadata$date == i, !is.na(datefilter[,i]) & datefilter[,i] == FALSE] <- 0
  }
  return(sampletable)
}
