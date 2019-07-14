#' Filter on occurence in controls
#'
#' This function allows you to filter compounds based on whether they are present in the ambient controls.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @keywords ambient floral filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_ambient_presence(chemtable, sampletable, metadata)
#' @export
filter_ambient_presence<-function(chemtable,sampletable,metadata){
  #subset by which samples are ambient
  Am<-sampletable[which(metadata$type=="ambient"),]
  #now find which columns in this subset occur in one or more ambient samples, which will give the column a sum greater than 0
  Am.2<-Am[,colSums(Am)>0]
  #make a vector of the names of these compounds
  Ambient<-colnames(Am.2)

  return(within(chemtable, {
    filter_ambient_presence <- factor(ifelse(name %in% Ambient,"InAmbient", "OK"))
  }))

}
