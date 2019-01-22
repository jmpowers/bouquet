#' A function to filter on retention times
#'
#' This function allows you to filter compounds that elute before a minimum retention time and/or after a maximum retention time.
#' @param chemtable the data frame of the data about the compounds
#' min the minimum retention time Defaults to xxx
#' max the maximum retention time Defaults to xxx
#' @keywords retention time filter
#' @export
#' @examples
#' filter_RT()


##load packages as needed

##just to be able to play with this:

#chemtable<-read.csv("dummy.csv", header=TRUE)

#chemtable$filter_RT<-vector("character", length=nrow(chemtable))

filter_RT<-function(chemtable,min,max){
  return(within(chemtable, {
    filter_RT <- factor(ifelse(RT < max & RT > min , "OK", ifelse(RT > max, "High", "Low")))
    }))
}

##this isn't actually printing anything!!

