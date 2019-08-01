#' Filter on significant differences from controls
#'
#' This function allows you to filter compounds based on whether the quantity of a compound in ambient or blank samples vs. floral samples is significantly different.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param alpha maximum acceptable p-value Defaults to XXX
#' @param adjust "fdr" for False Discovery Rate correction, "none" for no correction, or any other method of p.adjust
#' @param compare the type of sample to compare against - ambient or blank
#' @keywords ambient floral filter
#' @examples
#' filter_ambient_ttest(chemtable,sampletable,metadata,0.05)
#' @export
filter_ambient_ttest<-function(chemtable,sampletable,metadata,alpha,adjust = "none", compare = "ambient"){

  nvol<-length(sampletable)
  ts <-sapply(1:nvol,function(x) (t.test((sampletable[which(metadata$type==compare),x]), sampletable[which(metadata$type=="floral"),x])))

  Pvalue <- t(as.matrix(ts[3,]))
  if(adjust != "none") {
    Pvalue <- p.adjust(Pvalue, method = adjust)
  }

  filtername <- paste("filter",compare,"ttest", sep="_")
  countname <- paste("count",compare, sep=".")

  chemtable[,filtername] <- within(chemtable, {
  filter_ttest <- ifelse(Pvalue < alpha,"OK", "tTestFail")
  filter_ttest <- ifelse(get(countname)==0 & count.floral!=0, "OK",  filter_ttest)
  filter_ttest <- ifelse(get(countname)==0 & count.floral==0, "tTestZeros",  filter_ttest)
  filter_ttest <- ifelse(get(countname)!=0 & count.floral==0, "tTestFail",  filter_ttest)
  filter_ttest <- ifelse(get(countname)!=0 & count.floral!=0 & is.na(filter_ttest) | is.nan(filter_ttest), "tTestSmallN",  filter_ttest)
  filter_ttest <- factor(filter_ttest)})$filter_ttest

  return(chemtable)
}
