source('R/rating.R')
#library(rating)
library(data.table)

dt <- data.table(id=1:20, category=sample(c('C1','C2'),10,replace=T),value1=rnorm(20), value2=rnorm(20))
#dt <- data.table(id=1:20, category=c('C1',rep('C2',19)),value1=rnorm(20), value2=rnorm(20))

weight <- getWeight('example/system.csv')

# CASE 1:
dt.index <- getStandlized(dt, 3:4, 1)

dt.index <- getIndexSystem(dt.index, weight, 1)

dt.index <- getRatingResult(dt.index, c('overall'))

dt.para <- getRulerPara(dt, 3:4)
dt.index2 <- computeFromRuler(dt, 1, dt.para)

# CASE 2:
dt.index <- getStandlized(dt, 3:4, 1, 2)

dt.index <- getIndexSystem(dt.index, weight)

dt.index <- getRatingResult(dt.index, c('value1','value2','overall'), 2)

dt.para <- getRulerPara(dt, 3:4, 2)
dt.index2 <- computeFromRuler(dt[1], 1, dt.para, dt[1]$category)
dt.index2 <- getIndexSystem(dt.index2, weight)

## the bins for the ruler should be firstly compute OR directly given for absolute evaluation
bins <- getRulerBins(dt.index[category==dt[1]$category], 'overall')
dt.index2 <- getRatingResult(dt.index2, c('overall'), bins=bins)

## validate the multi-index system, the larger the better
remark <- getDiscriminationIndex(dt.index, "overall")
