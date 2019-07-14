#' Filter on ratios between samples and controls
#'
#' This function allows you to filter compounds based on a threshold ratio of a compound's mean area in floral samples vs. ambient samples
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param ratio the minimum ratio of mean areas from floral to ambient samples
#' @keywords ambient floral filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_ambient_ratio(chemtable, sampletable, metadata, ratio=10)
#' @export
filter_ambient_ratio<-function(chemtable, sampletable, metadata, ratio=3){
  attr(chemtable, "ambient_ratio") <- ratio
  nvol<-ncol(sampletable)
  #JP: I split the result into two columns - one for the ratio and one for whether it passed
  return(within(chemtable, {

    ambient_ratio <- sapply(1:nvol, function(x) {
      SampMean <- mean(sampletable[which(metadata$type=="floral"),x]);
      AmbiMean <- mean(sampletable[which(metadata$type=="ambient"),x]);
      return(ifelse(SampMean==0, 0, ifelse(AmbiMean==0, Inf, SampMean/AmbiMean)))  })

    filter_ambient_ratio <- factor(ifelse(ambient_ratio > ratio,"OK", "AmRatioFail"))
  }))

}
