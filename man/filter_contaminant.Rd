% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_contaminant.R
\name{filter_contaminant}
\alias{filter_contaminant}
\title{Filter known contaminants}
\usage{
filter_contaminant(chemtable, cont.list)
}
\arguments{
\item{chemtable}{the data frame of the data about the compounds}

\item{cont.list}{a list of known contaminants Defaults to our default list}
}
\description{
This function allows you to filter compounds that are known contaminants.
}
\examples{
data(GCMSfloral)
chemtable <- filter_contaminant(chemtable, cont.list = c("Caprolactam","Decanal"))
}
\keyword{contaminant}
\keyword{filter}
