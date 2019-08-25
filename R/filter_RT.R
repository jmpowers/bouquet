#' Filter on retention times
#'
#' This function allows you to filter out compounds that elute before a RT_minimum retention time and/or after a RT_maximum retention time.
#' @param chemtable the data frame of the data about the compounds
#' @param RT_min the RT_minimum retention time
#' @param RT_max the RT_maximum retention time
#' @keywords retention time filter
#' @examples
#' data(GCMSfloral)
#' chemtable <- filter_RT(chemtable, 4, 15)
#' @export
filter_RT<-function(chemtable, RT_min, RT_max){
  attr(chemtable, "RT_min") <- RT_min
  attr(chemtable, "RT_max") <- RT_max
  return(within(chemtable,
         filter_RT <- factor(ifelse(RT <= RT_max & RT >= RT_min , "OK", ifelse(RT >= RT_max, "High", "Low")))))
}
