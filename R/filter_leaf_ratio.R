#' A function to filter on whether the quantity of a compound in leaf samples vs. floral samples is significantly different
#'
#' This function allows you to filter compounds based on whether the quantity of a compound in leaf samples vs. floral samples is significantly different by comparing the ratio of a compound in the floral samples to the leaf samples.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param ratio the minimum ratio of mean areas from floral to ambient samples Defaults to XXX
#' @param leafgroup the name of the column that shows how leaf samples are related to floral samples
#' @keywords leaf floral filter
#' @export
#' @examples
#' filter_leaf_ratio()


#######chemtable is included in this but I'm not sure where it's supposed to come into play


filter_leaf_ratio<-function(chemtable,sampletable,metadata,ratio,leafgroup){
  nvol<-length(sampletable)
  #nvol should be the number of columns in the sample table

  for (i in unique(leafgroup)){
  avgd <- sapply(1:nvol,function(x) (mean(sampletable[which(metadata$type=="floral"),x])> ratio*mean(sampletable[which(metadata$type=="leaf"),x])))
  ##I don't know if this will work or if we will need to combine the sampletable and the metadata into the same dataframe for the purposes of this portion of the function
  }

  #for each leaf group (population), we want to know if the amount in the floral samples from that group are more than the leaf samples.
  #then we need to store that information about each compound in each group and then reference that when making our boolean for the whole chemtable below...except maybe this filtering information can't go in the chemtable, because a given compound could have a number of different values depending on the number of populations and the variability across them
  return(within(chemtable, {
    filter_leaf_ratio <- factor(ifelse(avgd==TRUE,"OK", "LeafRatioFail"))
  }))

}
