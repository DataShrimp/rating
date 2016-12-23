\name{getRatingResult}
\alias{getRatingResult}
\title{get rating results from the index data}
\usage{
dt.index <- getRatingResult(dt, rate_range, category_colnum=NA, bins=NA)
}
\description{
This function is used to rate the specific indexes defined by parameter 'rate_range'.
The parameter bins is used to assign the specific ruler field using function 'getRulerBins'
Or, the parameter bins can be defined for absolute evaluation.
The result will get 'A, B, C, D, E' according to the confidence interval of normal distribution.
}
\examples{
NA
}
