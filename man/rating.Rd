\name{rating}
\alias{rating}
\title{multi-index rating package}
\usage{
1. clean and prepare data (or mannually compute the basic index here)
# note that please convert the data.frame to data.table
dt <- fread(xxx)

2. get the index system and its weight (ensure the definition is consistent with prepared data)
# input: index system which is designed beforehand, fields formated as <pathString, weight, ...>
weight <- getWeight('xxx.csv')

3. compute the basic index (if the first step alread did, ignore this step)
# note that the index system file should have the consistent columns of 'func' and 'parameter'
# support functions include: portion, yoy, qoq, monthVol, yearVol, seasonVol, monthHhi

4. standandlize the index according to the proper category
# input: input data.table, index column, id column, category column, (both number or name are ok)
dt.index <- getStandlized(dt, c(3:10), 1, 2)

5. compute the derived index using proper aggragative model
# input: index data, weight data, category colunm (can be ignored if no classification)
dt.index <- getIndexSystem(dt.index, weight, 2)

6. get the rating results
# input: index data, allowed na numbers (exceedance will not rate)
dt.index <- getRatingResult(dt.index, 2)
}
\description{
This package is designed to write the multi-index rating algorithm with any business model effectively.
}
\examples{
# CASE 1: there is no category info in data set
library(rating)
library(data.table)

dt <- data.table(id=1:10, value1=rnorm(10), value2=rnorm(10))

# system.csv content:
#pathString,weight,func,parameter
#overall,,,
#overall/value1,0.5,,
#overall/value2,0.5,,
weight <- getWeight('example/system.csv')

dt.index <- getStandlized(dt, 2:3, 1)

dt.index <- getIndexSystem(dt.index, weight, 1)

dt.index <- getRatingResult(dt.index, c('overall'))

dt.para <- getRulerPara(dt, 2:3)
dt.index2 <- computeFromRuler(dt, 1, dt.para)

# ----------
# CASE 2: there is category info in data set
library(rating)
library(data.table)

dt <- data.table(id=1:20, category=sample(c('C1','C2'),10,replace=T),value1=rnorm(20), value2=rnorm(20))

# system.csv content:
#pathString,weight,func,parameter
#overall,,,
#overall/value1,0.5,,
#overall/value2,0.5,,
weight <- getWeight('example/system.csv')

dt.index <- getStandlized(dt, 3:4, 1, 2)

dt.index <- getIndexSystem(dt.index, weight)

dt.index <- getRatingResult(dt.index, c('overall'), 2)

dt.para <- getRulerPara(dt, 3:4, 2)
dt.index2 <- computeFromRuler(dt[1], 1, dt.para, dt[1]$category)
dt.index2 <- getIndexSystem(dt.index2, weight)

## the bins for the ruler should be firstly compute OR directly given for absolute evaluation
bins <- getRulerBins(dt.index[category==dt[1]$category], 'overall')
dt.index2 <- getRatingResult(dt.index2, c('overall'), bins=bins)

## validate the multi-index system, the larger the better
remark <- getDiscriminationIndex(dt.index, "overall")

}
