#' Filter on ratios between samples and controls by date
#'
#' This function allows you to filter compounds based on a threshold ratio of a compound's mean area in floral samples vs. ambient samples on a particular date.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param ratio the minimum ratio of mean areas from floral to the ambient sample on that collection date
#' @keywords ambient floral filter
#' @return datefilter, a dataframe of dates x compounds, containing boolean include_ambient_ratio
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_ambient_date(chemtable, sampletable, metadata, ratio=10)
#' @export
filter_ambient_date<-function(chemtable, sampletable, metadata, ratio=3){
  attr(chemtable, "ambient_ratio") <- ratio
  nvol<-ncol(sampletable)
  #JP: I split the result into two columns - one for the ratio and one for whether it passed
  metadata <- metadata[!is.na(metadata$date),]
  metadata$date <- factor(metadata$date)
  ambient_ratio <- data.frame(matrix(rep(0, nlevels(metadata$date) * nrow(chemtable)), ncol = nlevels(metadata$date), nrow = nrow(chemtable)))
  colnames(ambient_ratio) <- levels(metadata$date)
  rownames(ambient_ratio) <- chemtable$name
    for (i in unique(metadata$date)){
      if(length(which(metadata$type=="floral" & metadata$date==i)) == 0 |
         length(which(metadata$type=="ambient" & metadata$date==i)) == 0) {
        ambient_ratio[,i] <- NA
        print(paste("bad!!!",i))
      } else {
      ambient_ratio[,i] <- sapply(1:nvol, function(x) {
        SampMean <- mean(sampletable[which(metadata$type=="floral" & metadata$date==i),x]);
        AmbiMean <- mean(sampletable[which(metadata$type=="ambient"  & metadata$date==i),x]);
        return(ifelse(SampMean==0, 0, ifelse(AmbiMean==0, Inf, SampMean/AmbiMean)))  })
    }
    }
    datefilter <- ambient_ratio > ratio
    return(datefilter)
}
