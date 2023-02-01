#' Filter on significant differences from controls
#'
#' This function allows you to filter compounds based on whether the quantity of a compound in ambient or blank samples vs. floral samples is significantly different. It creates a filter_ambient_ttest column with the following values: OK (passed t-test), tTestFail (did not pass t-test), tTestZeros (cannot run t-test, data are all zeros), tTestSmallN (nonzero data but t-test returned NA or NaN). The (adjusted) p-value is added in the ambient_pvalue column.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param alpha maximum acceptable p-value, defaults to 0.05
#' @param adjust "fdr" for False Discovery Rate correction, "none" for no correction, or any other method of p.adjust
#' @param compare the type of sample to compare against - ambient or blank
#' @keywords ambient floral filter
#' @examples
#' filter_ambient_ttest(chemtable,sampletable,metadata,0.05)
#' @export
filter_ambient_ttest<-function(chemtable,sampletable,metadata,alpha,adjust = "none", compare = "ambient"){
  attr(chemtable, "alpha") <- alpha
  nvol<-length(sampletable)

  pvaluename <- paste(compare,"pvalue", sep="_")
  chemtable[,pvaluename] <- sapply(1:nvol,function(x) (t.test((sampletable[which(metadata$type==compare),x]),
                                                               sampletable[which(metadata$type=="floral"),x]))$p.value)
  if(adjust != "none") {
    chemtable[,pvaluename] <- p.adjust(chemtable[,pvaluename], method = adjust)
  }

  filtername <- paste("filter",compare,"ttest", sep="_")
  countname <- paste("count",compare, sep=".")

  chemtable[,filtername] <- within(chemtable, {
  filter_ttest <- ifelse(get(pvaluename) < alpha,"OK", "tTestFail")
  filter_ttest <- ifelse(get(countname)==0 & count.floral!=0, "OK",  filter_ttest)
  filter_ttest <- ifelse(get(countname)==0 & count.floral==0, "tTestZeros",  filter_ttest)
  filter_ttest <- ifelse(get(countname)!=0 & count.floral==0, "tTestFail",  filter_ttest)
  filter_ttest <- ifelse(get(countname)!=0 & count.floral!=0 & is.na(filter_ttest) | is.nan(filter_ttest), "tTestSmallN",  filter_ttest)
  filter_ttest <- factor(filter_ttest)})$filter_ttest

  return(chemtable)
}
