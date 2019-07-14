#' A function to filter on whether the quantity of a compound in leaf samples vs. floral samples is significantly different
#'
#' This function allows you to filter compounds based on whether the quantity of a compound in leaf samples vs. floral samples is significantly different by comparing the ratio of a compound in the floral samples to the leaf samples.
#' @param chemtable the data frame of the data about the compounds
#' @param sampletable the wide data frame with samples in rows and compound names in columns, containing peak areas
#' @param metadata the data frame that contains meta data about the group, type, and other attributes of each sample
#' @param ratio the minimum ratio of mean areas from floral to ambient samples Defaults to XXX
#' @param leafgroup the name of the column in the metadata that shows how leaf samples are related to floral samples
#' @keywords leaf floral filter
#' @export
#' @examples
#' filter_leaf_ratio()


#######chemtable is included in this but I'm not sure where it's supposed to come into play


filter_leaf_ratio<-function(chemtable,sampletable,metadata,ratio,leafgroup){
  nvol<-length(sampletable)
  #nvol should be the number of columns in the sample table


  if(!is.null(leafgroup)) {

    leaf_ratio <- data.frame(matrix(rep(0, nlevels(metadata[,leafgroup]) * nrow(chemtable)), ncol = nlevels(metadata[,leafgroup]), nrow = nrow(chemtable)))
    colnames(leaf_ratio) <- levels(metadata[,leafgroup])
    rownames(leaf_ratio) <- chemtable$name
    for (i in unique(metadata[,leafgroup])){
      if(length(which(metadata$type=="floral" & metadata[,leafgroup]==i)) == 0 |
         length(which(metadata$type=="leaf" & metadata[,leafgroup]==i)) == 0) {
        leaf_ratio[,i] <- NA
        print(paste("bad!!!",i))
      } else {
        leaf_ratio[,i] <- sapply(1:nvol, function(x) {
          SampMean <- mean(sampletable[which(metadata$type=="floral" & metadata[,leafgroup]==i),x]);
          LeafMean <- mean(sampletable[which(metadata$type=="leaf"  & metadata[,leafgroup]==i),x]);
          return(ifelse(SampMean==0, 0, ifelse(LeafMean==0, Inf, SampMean/LeafMean)))  })
      }
    }
    leaffilter <- leaf_ratio > ratio
    return(leaffilter)

  } else {
    leaf_ratio <- sapply(1:nvol, function(x) {
      SampMean <- mean(sampletable[which(metadata$type=="floral"),x]);
      LeafMean <- mean(sampletable[which(metadata$type=="leaf"),x]);
      return(ifelse(SampMean==0, 0, ifelse(LeafMean==0, Inf, SampMean/LeafMean)))  })

    return(within(chemtable, {
      filter_leaf_ratio <- factor(ifelse(leaf_ratio==TRUE,"OK", "LeafRatioFail"))
    }))
  }
  ##I don't know if this will work or if we will need to combine the sampletable and the metadata into the same dataframe for the purposes of this portion of the function

#for each leaf group (population), we want to know if the amount in the floral samples from that group are more than the leaf samples.

}
