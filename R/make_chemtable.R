#' Make chemical table
#'
#' Summarize information about each chemical, including retention time and frequency of occurence by groups. Prerequisite for the filter* functions.
#'
#' Generates chemtable, a data frame of the compounds, with the following columns:
#' \describe{
#' \item{compound}{compound name}
#' \item{RT}{mean RT}
#' \item{RT.var}{variance of RT}
#' \item{match}{mean match}
#' \item{match.var}{variance of match}
#' \item{max.area}{maximum area in floral samples}
#' \item{mean.area}{mean area in floral samples, including zeros}
#' \item{count.X}{multiple columns, number of occurrences within types and treatment groups}
#' \item{freq.X}{multiple columns, proportion of occurrences within types and treatment groups}
#' }
#' @param longdata the long format data frame
#' @param metadata the sample metadata
#' @return chemtable, a data frame of the compound
#' @examples
#' data(GCMSfloral)
#' \dontrun{chemtable <- make_chemtable(longdata, metadata)}
#' @export
make_chemtable <- function(longdata, metadata) {

  sampletable <- bouquet::make_sampletable(longdata)
  chemtable <- data.frame(name=levels(longdata$name))
  chemtable <- within(chemtable, {
    RT <-     sapply(name, function(x) {median(longdata$RT[longdata$name==x])})
    RT.var <- sapply(name, function(x) {var(longdata$RT[longdata$name==x])})
    match <-  sapply(name, function(x) {median(longdata$match[longdata$name==x])})
    match.var <- sapply(name, function(x) {var(longdata$match[longdata$name==x])})
    max <-    sapply(sampletable[metadata$type=="floral",], max)
    mean <-   sapply(sampletable[metadata$type=="floral",], mean)
  })

  add_counts_freqs <- function(chemtable, sampletable, groups) {
    for(g in levels(groups)[levels(groups)!=""]) { #"" are probably the ambients and blanks
      chemtable[,paste0("count.",g)] <- sapply(na.omit(sampletable[groups==g,]), function(x) sum(x>0))
      chemtable[,paste0("freq.",g)] <- sapply(na.omit(sampletable[groups==g,]), function(x) sum(x>0) /  length(x))
    }
    return(chemtable)
  }

  chemtable <- add_counts_freqs(chemtable, sampletable, metadata$type)
  for(grouping in attr(metadata, "group")) {
    chemtable <- add_counts_freqs(chemtable, sampletable, metadata[,grouping])
  }

  return(chemtable)
}
