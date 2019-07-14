#' Prune sampletable
#'
#'
#' @param chemtable the data frame of the data about the compounds
#' @keywords filters
#' @examples
#' data(GCMSfloral)
#' chemtable <- prune_sampletable(ampletable, chemtable, metadata)
#' @export
prune_sampletable<-function(sampletable, chemtable, metadata){
  return(sampletable[metadata$type == "floral", chemtable$filter_final])
}
