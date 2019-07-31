#' Filter on retention times
#'
#' This function allows you to filter out compounds that elute before a minimum retention time and/or after a maximum retention time.
#' @param chemtable the data frame of the data about the compounds
#' @param min the minimum retention time
#' @param max the maximum retention time
#' @keywords retention time filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_RT(chemtable, 4, 15)
#' @export
filter_RT<-function(chemtable, min, max){
  attr(chemtable, "RT_min") <- min
  attr(chemtable, "RT_max") <- max
  return(within(chemtable,
         filter_RT <- factor(ifelse(RT <= max & RT >= min , "OK", ifelse(RT >= max, "High", "Low")))))
}
