% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prune_sampletable.R
\name{prune_sampletable}
\alias{prune_sampletable}
\title{Prune sampletable}
\usage{
prune_sampletable(sampletable, chemtable, metadata)
}
\arguments{
\item{chemtable}{the data frame of the data about the compounds}
}
\description{
Prune sampletable
}
\examples{
data(GCMSfloral)
chemtable <- prune_sampletable(ampletable, chemtable, metadata)
}
\keyword{filters}
