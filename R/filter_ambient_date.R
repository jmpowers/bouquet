#' Filter on ratios between samples and controls by date
#'
#' This function allows you to filter compounds based on a threshold ratio of a compound's mean area in floral samples vs. ambient samples on a particular date.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param ratio the minimum ratio of mean areas from floral to the ambient sample on that collection date
#' @param prop_dates the minimum proportion of dates a compound must pass the ratio test to pass overall
#' @keywords ambient floral filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_ambient_date(chemtable, sampletable, metadata, ratio=4, prop_dates=0.2)
#' @export
filter_ambient_date<-function(chemtable, sampletable, metadata, ratio=3, prop_dates=0.5){
  attr(chemtable, "ambient_ratio") <- ratio
  nvol<-ncol(sampletable)
  metadata <- metadata[!is.na(metadata$date),]
  metadata$date <- factor(metadata$date)
  ambient_ratio <- data.frame(matrix(rep(0, nlevels(metadata$date) * nrow(chemtable)), ncol = nlevels(metadata$date), nrow = nrow(chemtable)))
  colnames(ambient_ratio) <- levels(metadata$date)
  rownames(ambient_ratio) <- chemtable$name
    for (i in unique(metadata$date)){
      if(length(which(metadata$type=="floral" & metadata$date==i)) == 0 |
         length(which(metadata$type=="ambient" & metadata$date==i)) == 0) {
        ambient_ratio[,i] <- NA
        message(paste("No controls or no samples to compare on",i))
      } else {
      ambient_ratio[,i] <- sapply(1:nvol, function(x) {
        SampMean <- mean(sampletable[which(metadata$type=="floral" & metadata$date==i),x]);
        AmbiMean <- mean(sampletable[which(metadata$type=="ambient"  & metadata$date==i),x]);
        return(ifelse(SampMean==0, 0, ifelse(AmbiMean==0, Inf, SampMean/AmbiMean)))  })
    }
    }
    datefilter <- ambient_ratio > ratio #boolean matrix of whether the compound passed on each date

    return(within(chemtable, {
      prop_dates_passed <- rowSums(datefilter, na.rm=T) / rowSums(!is.na(datefilter))
      filter_ambient_date <- ifelse(prop_dates_passed >= prop_dates, "OK", "dateFail")
    }))
}
