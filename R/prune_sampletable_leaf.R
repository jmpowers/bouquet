#' Prune sampletable by leaf
#' Zeros out cells in the sample table that did not pass the ambient leaf filter.
#'
#' @param chemtable the data frame of the data about the compounds
#' @keywords filters
#' @examples
#' data(GCMSfloral)
#' chemtable <- prune_sampletable_leaf(ampletable, chemtable, metadata, leafgroup, leaffilter)
#' @export
prune_sampletable_leaf<-function(sampletable, chemtable, metadata, leafgroup, leaffilter){
  for(i in colnames(leaffilter)) {
    sampletable[metadata$type == "floral" & metadata[,leafgroup] == i, !is.na(leaffilter[,i]) & leaffilter[,i] == FALSE] <- 0
  }
  return(sampletable)
}
