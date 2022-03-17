#' Floral volatiles of *Schiedea* hybrids
#'
#' GCMS_output includes the peak areas and identification of samples
#' from infloresences of *Schiedea kaalae*, *S. hookeri*, and reciprocal hybrids.
#' GCMS_metadata contains information about each sample.
#'
#' @format
#' GCMS_output is a data frame with 16217 rows and 5 variables:
#' \describe{
#'   \item{Sample}{name of the sample, ambient control, or blank}
#'   \item{RT}{retention time in minutes}
#'   \item{Name}{best-matching compound identified by NIST MS library search}
#'   \item{Area}{integrated area of the peak in counts}
#'   \item{Match}{match score between sample and library mass spectra (0-100)}
#' }
#' #' GCMS_metadata is a data frame with 183 rows and 7 variables:
#' \describe{
#'   \item{SampleDate}{date the sample was collected}
#'   \item{Filename}{name of the sample, ambient control, or blank}
#'   \item{StartSunset}{time of sample collection relative to sunset in hours}
#'   \item{Flrs}{number of open flowers in the bag}
#'   \item{Cross}{species (KAAL or HOOK) or hybrid cross (HOKA or KAHO) written with the maternal parent first}
#'   \item{Time}{time of day (Day or Night)}
#'   \item{Type}{type of sample (floral, ambient, blank)}
#' }
"GCMS_output"

#' @rdname GCMS_output
"GCMS_metadata"
