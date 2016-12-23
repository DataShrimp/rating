\name{getWeight}
\alias{getWeight}
\title{get weight info from an index system file or dataframe}
\usage{
weight = getWeight(filename)
or
weight = getweight(dataframe)
}
\description{
This function is used to read the desinged index system file or dataframe.
Note that the data format should be like this ([xxx] are columns for computing index automatically):
----
pathString, weight, [func], [parameter], [remark], ...
overall,,,,
overall/growth/yoy3, 0.3, yoy, 3, yoy index for 3 monthes
}
\examples{
NA
}
