#' Filter known contaminants
#'
#' This function allows you to filter compounds that are known contaminants.
#' @param chemtable the data frame of the data about the compounds
#' @param cont.list a list of known contaminants Defaults to our default list
#' @keywords contaminant filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_contaminant(chemtable, cont.list = c("Caprolactam","Decanal"))
#' @export
filter_contaminant<-function(chemtable,cont.list){
  return(within(chemtable, {
    filter_contaminant <- factor(ifelse(name %in% cont.list,"Contaminant", "OK"))
  }))
}
