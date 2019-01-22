#' Make chemical table
#'
#'
#' @param longdata the long format data frame
#' @param metadata the sample metadata
#' @return chemtable, a data frame of the compounds, with the following columns:
#' compound: compound name
#' RT: mean RT
#' RT.var: variance of RT
#' match: mean match
#' match.var: variance of match
#' max: maximum area
#' mean: mean area
#' count: number of occurrences across all samples
#' TODO count.X: multiple columns, number of occurrences within types and treatment groups
#' freq: proportion of occurrences across all samples
#' TODO freq.X: multiple columns, proportion of occurrences within types and treatment groups
#' @examples
#' chemtable <- make_chemtable(longdata, metadata)
#' @export

make_chemtable <- function(longdata, metadata) {
  sampletable <- bouquet::make_sampletable(longtable)
  chemtable <- within(data.frame(name=data$Name), {
    RT <-     sapply(name, function(x) {median(data$RT[data$name==x])})
    RT.var <- sapply(name, function(x) {var(data$RT[data$name==x])})
    match <-  sapply(name, function(x) {median(data$Match[data$name==x])})
    match.var <- sapply(name, function(x) {var(data$match[data$name==x])})
    max <-    sapply(sampletable[metadata$type=="floral",], max)
    mean <-   sapply(sampletable[metadata$type=="floral",], mean)
    count <-  sapply(sampletable[metadata$type=="floral",], function(x) sum(x>0) )
    freq <-  sapply(sampletable[metadata$type=="floral",], function(x) sum(x>0) / length(x) )
  })
  return(chemtable)
}
