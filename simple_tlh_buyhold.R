library(quantmod)

### harvesting functions ----
# horribly unoptimized and slow
harvest <- function(dat = NULL, threshold = 0.1, portval = 100000) {
  dat_monthly <- to.monthly(dat, indexAt="yearmon")
  nrw = nrow(dat_monthly)
  accum <- data.frame(
    date = index(dat_monthly),
    basis = rep(0, nrw),
    gl = rep(0, nrw),
    price = rep(0, nrw),
    portval = rep(0, nrw),
    quantity = rep(0, nrw),
    unrealgl = rep(0, nrw),
    yeargl = rep(0, nrw),
    yearstartprice = rep(0, nrw),
    yearret = rep(0, nrw)
  )

  for(i in seq_len(nrow(dat_monthly))) {
    rw = dat_monthly[i,]
  
    if(i == 1L) { # initialize first trade
      accum[i,"basis"] <- rw[,6,drop=TRUE]
      accum[i,"price"] <- rw[,6,drop=TRUE]
      accum[i,"portval"] <- portval
      accum[i,"quantity"] <- portval / rw[,6,drop=TRUE]
      accum[i,"yearstartprice"] <- rw[,6,drop=TRUE]
    } else { # evaluate tax loss harvesting opportunity
      lastrw <- accum[i - 1, ]
      accum[i,"basis"] <- lastrw[,"basis"]
      accum[i,"gl"] <- 0
      accum[i,"price"] <- rw[,6,drop=TRUE]
      accum[i,"portval"] <- lastrw[,"portval"] * (accum[i,"price"] / lastrw[,"price"])
      accum[i,"quantity"] <- lastrw[,"quantity"]
      accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
      accum[i,"yearstartprice"] <- accum[i-1,"yearstartprice"]
      # harvest if price below threshold
      if(rw[,6,drop=TRUE] < lastrw$basis * (1 - threshold)) {
        accum[i,"basis"] <- rw[,6,drop=TRUE]
        accum[i,"gl"] <- (lastrw[,"quantity"] * rw[,6,drop=TRUE]) - lastrw[,"portval"]
        accum[i,"quantity"] <- accum[i,"portval"] / rw[,6,drop=TRUE]
        accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
      }
      accum[i,"unrealgl"] <- accum[i, "portval"] - (accum[i, "quantity"] * accum[i, "basis"])
      # beginning of year reset yeargl
      if(months(accum[i,"date"]) == "January"){
        accum[i,"yeargl"] <- accum[i,"gl"]
        accum[i,"yearstartprice"] <- accum[i-1,"price"]
      }
      accum[i-1,"yearret"] <- accum[i-1,"price"] / accum[i-1,"yearstartprice"] - 1
    }
  }
  
  accum
}


test_harvest_yearly <- function(dat = NULL, ...) {
  yrs <- unique(format(index(dat),"%Y"))
  structure(lapply(yrs, function(yr) {
    dat_yr <- dat[paste0(yr,"::"),]
    harvest(dat = dat_yr, ...)
  }), names = yrs)
}

evaluate_harvesting <- function(dat = NULL) {
  as.data.frame(
    do.call(
      rbind,
      lapply(dat, function(startyr) {
        c(
          startyr = as.numeric(format(startyr[1,"date"], "%Y")),
          nharvests = length(which(startyr$gl != 0)),
          gl_total = sum(startyr$gl)
        )
      })
    )
  )
}




### try out our harvesting functions ----
quantmod::getSymbols("^GSPC", from="1900-01-01")

h <- test_harvest_yearly(GSPC["2010::",])

eh <- evaluate_harvesting(h)

barplot(eh$gl_total~eh$startyr)
barplot(eh$nharvests~eh$startyr)
