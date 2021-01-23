library(quantmod)

quantmod::getSymbols("^GSPC", from="1900-01-01")

GSPC_monthly <- to.monthly(GSPC, indexAt="yearmon")

accum <- data.frame()
nextrw <- 1
threshold <- 0.1
portval <- 100000

for(i in seq_len(nrow(GSPC_monthly))) {
  rw = GSPC_monthly[i,]

  if(nrow(accum) == 0L) { # initialize first trade
    accum[nextrw,"date"] <- as.Date(index(rw))
    accum[nextrw,"basis"] <- rw[,6,drop=TRUE]
    accum[nextrw,"gl"] <- NA
    accum[nextrw,"price"] <- rw[,6,drop=TRUE]
    accum[nextrw,"portval"] <- portval
    accum[nextrw,"quantity"] <- portval / rw[,6,drop=TRUE]
    accum[nextrw,"yeargl"] <- 0
    nextrw <<- nextrw + 1
  } else { # evaluate tax loss harvesting opportunity
    lastrw <- accum[nextrw - 1, ]
    accum[nextrw,"date"] <- as.Date(index(rw))
    accum[nextrw,"basis"] <- lastrw[,"basis"]
    accum[nextrw,"gl"] <- 0
    accum[nextrw,"price"] <- rw[,6,drop=TRUE]
    accum[nextrw,"portval"] <- lastrw[,"portval"] * (accum[nextrw,"price"] / lastrw[,"price"])
    accum[nextrw,"quantity"] <- lastrw[,"quantity"]
    accum[nextrw,"yeargl"] <- lastrw[,"yeargl"] + accum[nextrw,"gl"]
    if(rw[,6,drop=TRUE] < lastrw$basis * (1 - threshold)) {
      accum[nextrw,"date"] <- as.Date(index(rw))
      accum[nextrw,"basis"] <- rw[,6,drop=TRUE]
      accum[nextrw,"gl"] <- (lastrw[,"quantity"] * rw[,6,drop=TRUE]) - lastrw[,"portval"]
      accum[nextrw,"quantity"] <- accum[nextrw,"portval"] / rw[,6,drop=TRUE]
      accum[nextrw,"yeargl"] <- lastrw[,"yeargl"] + accum[nextrw,"gl"]
    }
    # beginning of year reset yeargl
    if(months(accum[nextrw,"date"]) == "January"){
      accum[nextrw,"yeargl"] <- accum[nextrw,"gl"]
    }
    nextrw <<- nextrw + 1
  }
}

plot(accum$portval ~ accum$date, type="l")
