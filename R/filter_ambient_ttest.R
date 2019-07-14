#' Filter on significant differences from controls
#'
#' This function allows you to filter compounds based on whether the quantity of a compound in ambient samples vs. floral samples is significantly different.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param alpha maximum acceptable p-value Defaults to XXX
#' @param adjust "fdr" for False Discovery Rate correction, "none" for no correction, or any other method of p.adjust
#' @keywords ambient floral filter
#' @examples
#' filter_ambient_ttest()
#' @export
filter_ambient_ttest<-function(chemtable,sampletable,metadata,alpha,adjust = "none"){
  nvol<-length(sampletable)
#nvol should be the number of columns in the sample table
  ts <-sapply(1:nvol,function(x) (t.test((sampletable[which(metadata$type=="ambient"),x]), sampletable[which(metadata$type=="floral"),x])))
##I don't know if this will work or if we will need to combine the sampletable and the metadata into the same dataframe for the purposes of this portion of the function
Pvalue <- t(as.matrix(ts[3,]))
if(adjust != "none") {
  Pvalue <- p.adjust(Pvalue, method = adjust)
}
#print(head(ts))
  return(within(chemtable, {
    filter_ambient_ttest <- ifelse(Pvalue < alpha,"OK", "tTestFail")
    filter_ambient_ttest <- ifelse(count.ambient==0 & count.floral!=0, "OK",  filter_ambient_ttest)
    filter_ambient_ttest <- ifelse(count.ambient==0 & count.floral==0, "tTestZeros",  filter_ambient_ttest)
    filter_ambient_ttest <- ifelse(count.ambient!=0 & count.floral==0, "tTestFail",  filter_ambient_ttest)
    filter_ambient_ttest <- ifelse(count.ambient!=0 & count.floral!=0 & is.na(filter_ambient_ttest) | is.nan(filter_ambient_ttest), "tTestSmallN",  filter_ambient_ttest)
    filter_ambient_ttest <- factor(filter_ambient_ttest)
  }))
#also need an option for when p.adjust is not "none"
}
