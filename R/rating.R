
# -----------------------
# read weight functions
getWeight <- function(input) {
  # construct system from a data table (e.g. a csv file)
  if (class(input) == "character")
    dt <- fread(input)
  else
    dt <- as.data.table(input)

  # prepare data for automatic
  getLevel <- function(x) {
    return (length(unlist(x)))
  }

  getGroup <- function(x) {
    a <- unlist(x)
    if (length(a) == 1)
      return (NA)
    return (a[length(a)-1])
  }

  getName <- function(x) {
    a <- unlist(x)
    return (a[length(a)])
  }

  dt$level <- unlist(lapply(strsplit(dt$pathString, '/'), FUN=getLevel))
  dt$group <- unlist(lapply(strsplit(dt$pathString, '/'), FUN=getGroup))
  dt$name <- unlist(lapply(strsplit(dt$pathString, '/'), FUN=getName))
  # weight <- dt[order(-dt$level)]
  weight <- data.table::setorder(dt, -level)

  return(weight)
}

# -----------------------
# rating function
getRatingResult <- function(dt, rate_range, category_colnum=NA, bins=NA) {
  rating <- function(x, bins.=bins) {
    if (length(x) < 5 || sd(x, na.rm=T)==0 || is.na(sd(x, na.rm=T)))
      return (rep(NA, length(x)))

    if (is.na(bins))
      bins <- quantile(unique(x), probs=c(0,pnorm(-1.5),pnorm(-0.5),pnorm(0.5),pnorm(1.5),1), na.rm=T)
    temp <- cut(x, breaks=bins, labels=c('E','D','C','B','A'), include.lowest=T)
    return (temp)
  }

  if (is.na(category_colnum)) {
    for (i in rate_range) {
      tmp <- rating(unlist(dt[,i,with=F]), bins)
      dt <- cbind(dt, tmp)
      colnames(dt)[ncol(dt)] <- paste0(i, '_rate')
    }
  }
  else {
    setkeyv(dt, names(dt[,category_colnum,with=F]))
    setorderv(dt, names(dt[,category_colnum,with=F]))
    for (i in rate_range) {
      tmp <- vector()
      for (cate in unique(unlist(dt[,category_colnum,with=F]))) {
        tmp <- append(tmp, rating(unlist(dt[cate,i,with=F]), bins))
      }
      tmp <- as.factor(tmp)
      levels(tmp) <- c('E','D','C','B','A')
      dt <- cbind(dt, tmp)
      colnames(dt)[ncol(dt)] <- paste0(i, '_rate')
    }
  }

  return(dt)
}

# -----------------------
# index computing function
getIndexSystem <- function(dt, weight, NAnum=10) {
  # define compose strategy here
  customWeighted <- function(x, w) {
    tmp <- sum(is.na(x))
    if (tmp >= NAnum)
      ret <- NA
    else
      ret <- weighted.mean(x, w, na.rm=T)
    return(ret)
  }

  for (gname in unique(weight$group)) {
    if (is.na(gname))
      break
    tmp <- apply(dt[,weight[group==gname]$name,with=F], 1, customWeighted,
                 w = weight[group==gname]$weight)
    dt <- cbind(dt, tmp)
    colnames(dt)[ncol(dt)] <- gname
  }
  return(dt)
}

# -----------------------
# standardlize functions
getStandlized <- function(dt, index_colrange, id_colnum, category_colnum=NA) {
  normalize <- function(x) {
    return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  }

  sigmoid <- function(x) {
    return(1/(1+exp(-x)))
  }

  standardlize <- function(x) {
    # NOTE: no Inf or -Inf here (try to avoid inputing Inf values)
    x <- as.numeric(x)
    x[is.infinite(x)] <- NA

    if (sd(x, na.rm=T)==0 || is.na(sd(x, na.rm=T)))   # avoid cases when all values are equal
      ret <- ifelse(x==0, 0, 1)
    else
      ret <- normalize(sigmoid(scale(x)))
    return(ret)
  }

  if (is.na(category_colnum)) {
    ret <- cbind(dt[,id_colnum,with=F],
                 as.data.frame(lapply(dt[,index_colrange,with=F], FUN=standardlize)))
  }
  else {
    key_name <- colnames(dt)[category_colnum]
    setkeyv(dt, key_name)
    ret <- data.table(dt[0,id_colnum,with=F], dt[0,category_colnum,with=F],
                      dt[0,index_colrange,with=F])
    for (categ in unique(unlist(dt[,category_colnum,with=F]))) {
      tmp <- cbind(dt[categ,id_colnum,with=F], dt[categ,category_colnum,with=F],
                   as.data.frame(lapply(dt[categ,index_colrange,with=F], FUN=standardlize)))
      ret <- rbind(ret, tmp)
    }
  }
  return(ret)
}

# -----------------------
# ruler bins functions
getRulerBins <- function(dt.index, index_colnum) {
  bins <- quantile(dt.index[,index_colnum,with=F], probs=c(0,pnorm(-1.5),pnorm(-0.5),pnorm(0.5),pnorm(1.5),1), na.rm=T)
  return (bins)
}

# -----------------------
# ruler parameter functions
getRulerPara <- function(dt, index_colrange, category_colnum=NA) {
  sigmoid <- function(x) {
    return(1/(1+exp(-x)))
  }

  min.sigmoid <- function(x) {
    if (sd(x, na.rm=T)==0 || is.na(sd(x, na.rm=T)))
      return (x[1])
    else
      return (min(sigmoid(scale(x)), na.rm=T))
  }

  max.sigmoid <- function(x) {
    if (sd(x, na.rm=T)==0 || is.na(sd(x, na.rm=T)))
      return (x[1])
    else
      return (max(sigmoid(scale(x)), na.rm=T))
  }

  if (is.na(category_colnum)) {
    ret <- dt[,index_colrange,with=F][0]
    #  no ruler, if the data set size is smaller than 6
    #if (nrow(dt) < 6)
    #  return (ret)
    ret <- rbind(ret, lapply(dt[,index_colrange,with=F], min.sigmoid))
    ret <- rbind(ret, lapply(dt[,index_colrange,with=F], max.sigmoid))
    ret <- rbind(ret, lapply(dt[,index_colrange,with=F], mean, na.rm=T))
    ret <- rbind(ret, lapply(dt[,index_colrange,with=F], sd, na.rm=T))
  }
  else {
    key_name <- colnames(dt)[category_colnum]
    setkeyv(dt, key_name)
    ret <- data.table(dt[0,category_colnum,with=F], dt[0,index_colrange,with=F])
    colnames(ret)[1] <- 'category'
    for (categ in unique(unlist(dt[,category_colnum,with=F]))) {
      #  no ruler, if the data set size is smaller than 6
      #if (nrow(dt[categ]) < 6)
      #  next
      ret <- rbind(ret, data.frame(category=categ, lapply(dt[categ,index_colrange,with=F], min.sigmoid)))
      ret <- rbind(ret, data.frame(category=categ, lapply(dt[categ,index_colrange,with=F], max.sigmoid)))
      ret <- rbind(ret, data.frame(category=categ, lapply(dt[categ,index_colrange,with=F], mean, na.rm=T)))
      ret <- rbind(ret, data.frame(category=categ, lapply(dt[categ,index_colrange,with=F], sd, na.rm=T)))
    }
  }
  return (ret)
}

computeFromRuler <- function(dt, id_colnum, dt.para, category=NA) {
  sigmoid <- function(x) {
    return(1/(1+exp(-x)))
  }

  if (is.na(category)) {
    ret <- dt[,id_colnum,with=F]
    for (colname in colnames(dt.para)) {
      v1 <- as.numeric(dt.para[1,colname,with=F])
      v2 <- as.numeric(dt.para[2,colname,with=F])
      v3 <- as.numeric(dt.para[3,colname,with=F])
      v4 <- as.numeric(dt.para[4,colname,with=F])
      x <- dt[,colname, with=F]
      if (v4==0 || is.na(v4)) {
        ret <- cbind(ret, x)
      } else {
        a <- (x-v3)/v4
        b <- (sigmoid(a)-v1)/(v2-v1)
        ret <- cbind(ret, b)
      }
    }
  }
  else {
    ret <- dt[,id_colnum, with=F]
    for (colname in colnames(dt.para)) {
      if (colname == 'category')
        next
      v1 <- as.numeric(dt.para[category==category][1,colname,with=F])
      v2 <- as.numeric(dt.para[category==category][2,colname,with=F])
      v3 <- as.numeric(dt.para[category==category][3,colname,with=F])
      v4 <- as.numeric(dt.para[category==category][4,colname,with=F])
      x <- dt[,colname, with=F]
      if (v4==0 || is.na(v4)) {
        ret <- cbind(ret, x)
      } else {
        a <- (x-v3)/v4
        b <- (sigmoid(a)-v1)/(v2-v1)
        ret <- cbind(ret, b)
      }
    }
  }
  return (ret)
}

# -----------------------
# get the discrimination index (validate the multi-index rating system)
getDiscriminationIndex <- function(dt.index, index_colname) {
  setorderv(dt.index, index_colname, -1)
  tmps <- unlist(as.list(dt.index[,index_colname,with=F]))
  tmps <- tmps[!is.na(tmps)]

  tmps.max <- max(tmps, na.rm=T)
  tmps.min <- min(tmps, na.rm=T)

  total <- 0
  for (n in 2:length(tmps)) {
    tmp <- (tmps[n]-tmps[n-1])^2
    total <- total + (tmp + 1)^(1/2)
  }

  ret <- total / ((tmps.max-tmps.min)^2 + (length(tmps)-1)^2)^(1/2)
  return (ret)
}

# -----------------------
# index functions (not frequently used, always user-defined according to specific project)

.getNRecentMonth <- function(date, n) {
  # ensure no NA in the date list
  date <- date[complete.cases(date)]
  date.chosen <- max(date)
  n.interval <- -n
  bins <- seq(date.chosen, min(date)+n.interval*31, by=paste(n.interval,"month"))
  return(cut(date, breaks=bins, right=TRUE, labels=rev(1:(length(bins)-1))))
}

# safe division (support vector computing)
# NOTE: ensure all values > 0, NA will convert to 0, 0/0=0 but 1/0=NA (not Inf)
.safeDivide <- function(divisor, dividend) {
  divisor[is.na(divisor)] <- 0
  dividend[is.na(dividend)] <- 0
  ret = c()
  for (s in seq(1:length(divisor))) {
    temp <- 0
    if (length(dividend) != 1)
      temp <- ifelse(divisor[s]==0,0,(divisor[s]/dividend[s]))
    else
      temp <- ifelse(divisor[s]==0,0,(divisor[s]/dividend[1]))
    ret <- append(ret, temp)
  }
  ret[is.infinite(ret)] <- NA
  return (ret)
}

# safe sd function
.safeSd <- function(x) {
  if (length(x) <= 1) {
    return (0)
  }
  return (sd(x))
}

# portion index, Note: data frame format: (id, date, value, category? TODO?)
portion <- function(dt.portion, months) {
  colnames(dt.portion) <- c('id', 'date', 'value')
  if (months < 1) {
    print('input months cannot be smaller than 1 month')
    return
  }
  months.max <- as.numeric(floor((max(dt.portion$date) - min(dt.portion$date))/30))
  if (months > months.max) {
    print('input months cannot exceed the max month number of dataset')
    return
  }
  dt.portion$months <- .getNRecentMonth(dt.portion$date, months)
  dt.portion <- dt.portion[months=="1"][,sum(value),by=id]
  dt.portion[,V1:=.safeDivide(V1, sum(V1))]
  colnames(dt.portion) <- c('id', paste0('portion',months))
  return (dt.portion)
}

# yoy index, Note: data frame format: (id, date, value)
yoy <- function(dt.yoy, months) {
  colnames(dt.yoy) <- c('id', 'date', 'value')
  dt.yoy$month <- as.numeric(as.character(.getNRecentMonth(dt.yoy$date, 1)))
  section <- 1:months
  t1 <- dt.yoy[month %in% section][,sum(value),by='id']
  section <- section+12
  t2 <- dt.yoy[month %in% section][,sum(value),by='id']
  dt.yoy <- t1[t2, on='id']
  # NA assign to 0
  dt.yoy[is.na(dt.yoy)] <- 0
  dt.yoy <- dt.yoy[,yoy:=.safeDivide((V1-i.V1), i.V1)][,.(id, yoy)]
  colnames(dt.yoy) <- c('id', paste0('yoy', months))
  return (dt.yoy)
}

# qoq index, Note: data frame format: (id, date, value)
qoq <- function(dt.qoq, months) {
  colnames(dt.qoq) <- c('id', 'date', 'value')
  dt.qoq$month <- as.numeric(as.character(.getNRecentMonth(dt.qoq$date, 1)))
  section <- 1:months
  t1 <- dt.qoq[month %in% section][,sum(value),by='id']
  section <- section+months
  t2 <- dt.qoq[month %in% section][,sum(value),by='id']
  dt.qoq <- t1[t2, on='id']
  # NA assign to 0
  dt.qoq[is.na(dt.qoq)] <- 0
  dt.qoq <- dt.qoq[,qoq:=.safeDivide((V1-i.V1), i.V1)][,.(id, qoq)]
  colnames(dt.qoq) <- c('id', paste0('qoq', months))
  return (dt.qoq)
}

# yoy plus index, (A-B)/((A+B)/2), Note: data frame format: (id, date, value)
# make sure that all values > 0
yoyplus <- function(dt.yoy, months) {
  colnames(dt.yoy) <- c('id', 'date', 'value')
  dt.yoy$month <- as.numeric(as.character(.getNRecentMonth(dt.yoy$date, 1)))
  section <- 1:months
  t1 <- dt.yoy[month %in% section][,sum(value),by='id']
  section <- section+12
  t2 <- dt.yoy[month %in% section][,sum(value),by='id']
  dt.yoy <- t1[t2, on='id']
  # NA assign to 0
  dt.yoy[is.na(dt.yoy)] <- 0
  dt.yoy <- dt.yoy[,yoy:=.safeDivide((V1-i.V1), (V1+i.V1)/2)][,.(id, yoy)]
  colnames(dt.yoy) <- c('id', paste0('yoy', months))
  return (dt.yoy)
}

# qoq plus index, (A-B)/((A+B)/2), Note: data frame format: (id, date, value)
# make sure that all values > 0
qoqplus <- function(dt.qoq, months) {
  colnames(dt.qoq) <- c('id', 'date', 'value')
  dt.qoq$month <- as.numeric(as.character(.getNRecentMonth(dt.qoq$date, 1)))
  section <- 1:months
  t1 <- dt.qoq[month %in% section][,sum(value),by='id']
  section <- section+months
  t2 <- dt.qoq[month %in% section][,sum(value),by='id']
  dt.qoq <- t1[t2, on='id']
  # NA assign to 0
  dt.qoq[is.na(dt.qoq)] <- 0
  dt.qoq <- dt.qoq[,qoq:=.safeDivide((V1-i.V1), (V1+i.V1)/2)][,.(id, qoq)]
  colnames(dt.qoq) <- c('id', paste0('qoq', months))
  return (dt.qoq)
}

# volatity index, Note: data frame format: (id, date, value)
monthVol <- function(dt.vol, months) {
  colnames(dt.vol) <- c('id', 'date', 'value')
  dt.vol$month <- as.numeric(as.character(.getNRecentMonth(dt.vol$date, 1)))
  dt.vol$ymonth <- substr(as.character(dt.vol$date), 1, 7)
  dt.vol <- dt.vol[month %in% seq(1, months)][,sum(value),by=.(id,ymonth)]
  dt.vol <- dt.vol[,.safeDivide(.safeSd(V1), mean(V1)),by=id]
  colnames(dt.vol) <- c('id', paste0('vol', months, 'm'))
  return (dt.vol)
}

seasonVol <- function(dt.vol, seasons) {
  colnames(dt.vol) <- c('id', 'date', 'value')
  dt.vol$season <- as.numeric(as.character(.getNRecentMonth(dt.vol$date, 3)))
  dt.vol <- dt.vol[season %in% seq(1, seasons)][,sum(value),by=.(id,season)]
  dt.vol <- dt.vol[,.safeDivide(.safeSd(V1), mean(V1)),by=id]
  colnames(dt.vol) <- c('id', paste0('vol', seasons, 's'))
  return (dt.vol)
}

yearVol <- function(dt.vol, years) {
  colnames(dt.vol) <- c('id', 'date', 'value')
  dt.vol$year <- as.numeric(as.character(.getNRecentMonth(dt.vol$date, 12)))
  dt.vol <- dt.vol[year %in% seq(1, years)][,sum(value),by=.(id,year)]
  dt.vol <- dt.vol[,.safeDivide(.safeSd(V1), mean(V1)),by=id]
  colnames(dt.vol) <- c('id', paste0('vol', years, 'y'))
  return (dt.vol)
}

# hhi index, NOte: dataframe format: (id, date)
monthHhi <- function(dt.hhi, months) {
  month_diff <- function(x, start.date, end.date) {
    x <- append(x, c(start.date, end.date))
    x <- sort(x)
    diff <- (year(x)[length(x):2]-year(x)[(length(x)-1):1])*12
    diff <- diff + month(x)[length(x):2]-month(x)[(length(x)-1):1]
    return(diff)
  }

  hhi <- function(x) {
    x.sum <- sum(x)
    hhi <- sum((x/x.sum)^2)
    return(hhi)
  }

  colnames(dt.hhi)[1:2] <- c('id', 'date')
  dt.hhi$month <- as.numeric(as.character(.getNRecentMonth(dt.hhi$date, 1)))
  dt.hhi <- dt.hhi[month %in% seq(1:months)]
  start.date <- min(dt.hhi$date)
  end.date <- max(dt.hhi$date)
  dt.hhi <- dt.hhi[,hhi(month_diff(date, start.date, end.date)), by=id]
  colnames(dt.hhi) <- c('id', paste0('hhi', months))
  return (dt.hhi)
}
