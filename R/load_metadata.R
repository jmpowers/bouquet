#' Load metadata
#'
#' Loads metadata about the samples
#' @param metadata data frame of sample metadata
#' @param date the text date of sampling
#' @param sample the string that specifies the sample ID
#' @param group a vector of columns that specifies species or treatment groups
#' @param type the column that specifies what type of sample it is (e.g. floral, ambient control, etc.). Levels should be c("floral", "ambient", "leaf")
#' @param amount the string that specifies the amount used for standardization (e.g. flower number or mass)
#' @return metadata, a data frame with the standard column names and types
#' @examples
#' data(GCMSfloral)
#' metadata <- load_metadata(GCMS_metadata, "SampleDate", "Filename", c("Cross", "Time"), "Type", "Flrs")
#' @export
load_metadata <- function(metadata, date=NULL, sample, group=NULL, type, amount=NULL) {

  #rename columns
  cols <- as.list(environment())[c("date", "sample", "type", "amount")]
  cols <- cols[sapply(cols, is.character)]
  found <- match(colnames(metadata), cols)
  colnames(metadata) <- ifelse(is.na(found), colnames(metadata), names(cols)[found])

  #make sure that metadata is in the same order as sampletable
  metadata <- metadata[order(metadata$sample),]

  #add info about groups and make each group column a factor
  if(!is.null(group)) {
    metadata[,group] <- lapply(metadata[,group, drop=FALSE], as.factor)
  }
  attr(metadata,"group") <- as.vector(group)

  #check the type column
  metadata$type <- as.factor(metadata$type)
  recog <- levels(metadata$type) %in% c("floral", "ambient", "leaf")
  message(paste0("Recognized types: \'", paste(levels(metadata$type)[recog], collapse="\' \'"),
                "\'\nOther types: \'",paste(levels(metadata$type)[!recog], collapse="\' \'"),"\'"))
  if(any(is.na(metadata$type))) {
    stop("The type column cannot contain NA values")
  }

  return(metadata)
}
