% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_leaf_ratio.R
\name{filter_leaf_ratio}
\alias{filter_leaf_ratio}
\title{A function to filter on whether the quantity of a compound in leaf samples vs. floral samples is significantly different}
\usage{
filter_leaf_ratio(chemtable, sampletable, metadata, ratio, leafgroup)
}
\arguments{
\item{chemtable}{the data frame of the data about the compounds}

\item{sampletable}{the wide data frame with samples in rows and compound names in columns, containing peak areas}

\item{metadata}{the data frame that contains meta data about the group, type, and other attributes of each sample}

\item{ratio}{the minimum ratio of mean areas from floral to ambient samples Defaults to XXX}

\item{leafgroup}{the name of the column in the metadata that shows how leaf samples are related to floral samples}
}
\description{
This function allows you to filter compounds based on whether the quantity of a compound in leaf samples vs. floral samples is significantly different by comparing the ratio of a compound in the floral samples to the leaf samples.
}
\examples{
filter_leaf_ratio()
}
\keyword{filter}
\keyword{floral}
\keyword{leaf}
