library(quantmod)

### harvesting functions ----
# horribly unoptimized and slow
harvest <- function(dat = NULL, threshold = 0.1, portval = 100000) {
  dat_monthly <- to.monthly(dat, indexAt="yearmon")
  nrw = nrow(dat_monthly)
  # set up empty data.frame based on data which we will populate
  accum <- data.frame(
    date = index(dat_monthly),
    basis = rep(0, nrw),
    gl = rep(0, nrw),
    price = rep(0, nrw),
    portval = rep(0, nrw),
    unrealgl = rep(0, nrw),
    carryforward = rep(0, nrw),
    yeargl = rep(0, nrw),
    yearstartprice = rep(0, nrw),
    yearret = rep(0, nrw)
  )

  for(i in seq_len(nrow(dat_monthly))) {
    rw = dat_monthly[i,]
  
    if(i == 1L) { # initialize first trade
      accum[i,"basis"] <- portval
      accum[i,"price"] <- rw[,6,drop=TRUE]
      accum[i,"portval"] <- portval
      accum[i,"yearstartprice"] <- rw[,6,drop=TRUE]
    } else { # evaluate tax loss harvesting opportunity
      lastrw <- accum[i - 1, ]
      accum[i,"basis"] <- lastrw[,"basis"]
      accum[i,"gl"] <- 0
      accum[i,"price"] <- rw[,6,drop=TRUE]
      accum[i,"portval"] <- lastrw[,"portval"] * (accum[i,"price"] / lastrw[,"price"])
      accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
      accum[i,"yearstartprice"] <- lastrw[,"yearstartprice"]
      accum[i,"carryforward"] <- lastrw[,"carryforward"]
      # harvest if price below threshold
      if(accum[i,"portval"] < lastrw[, "basis"] * (1 - threshold )) {
        accum[i,"gl"] <- accum[i,"portval"] - accum[i,"basis"]
        accum[i,"basis"] <- accum[i,"portval"]
        accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
        accum[i,"carryforward"] <- accum[i-1,"carryforward"] + accum[i,"gl"]
      }
      accum[i,"unrealgl"] <- accum[i, "portval"] - accum[i, "basis"]
      # # offset gain if loss available
      # # I thought this made sense but in reality it does not
      # # for an isolated portfolio we can never get basis higher than starting portfolio value.
      # # Assuming a time series eventually moves to a cumulative gain then basis will
      # # approach and eventually equal starting portfolio value
      # # meaning that at the point where basis equals starting portfolio value
      # # there is no difference between the tax-loss-harvested portfolio and the buy-hold.
      # # In this case, we need another application for the harvested gains outside of
      # # the isolated portfolio to get any value from it.
      # if(accum[i,"unrealgl"] > 0 && lastrw[,"carryforward"] < 0) {
      #   availgl <- (-accum[i,"carryforward"]) / accum[i,"unrealgl"] 
      #   if(availgl > 1) {
      #     accum[i,"gl"] <- accum[i,"portval"] - accum[i,"basis"]
      #     accum[i,"basis"] <- accum[i,"portval"]
      #     accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
      #     accum[i,"unrealgl"] <- accum[i, "portval"] - accum[i,"basis"]
      #     accum[i,"carryforward"] <- lastrw[,"carryforward"] + accum[i,"gl"]
      #   } else {
      #     sellpct <- -accum[i,"carryforward"] / accum[i,"unrealgl"]
      #     accum[i,"gl"] <- -accum[i,"carryforward"]
      #     accum[i,"carryforward"] <- 0
      #     accum[i,"basis"] <- accum[i,"basis"] + accum[i,"gl"]
      #     accum[i,"unrealgl"] <- accum[i, "portval"] - accum[i,"basis"]
      #     accum[i,"yeargl"] <- lastrw[,"yeargl"] + accum[i,"gl"]
      #   }
      # }
      # beginning of year reset yeargl
      if(months(accum[i,"date"]) == "January"){
        accum[i,"yeargl"] <- accum[i,"gl"]
        accum[i,"yearstartprice"] <- lastrw[,"price"]
      }
      accum[i-1,"yearret"] <- lastrw[,"price"] / lastrw[,"yearstartprice"] - 1
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
          nharvests = length(which(startyr$gl < 0)),
          noffsets = length(which(startyr$gl > 0)),
          gl_total = sum(startyr$gl)
        )
      })
    )
  )
}




### try out our harvesting functions ----
quantmod::getSymbols("^GSPC", from="1900-01-01")

h <- test_harvest_yearly(GSPC)

eh <- evaluate_harvesting(h)

barplot(eh$gl_total~eh$startyr)
barplot(eh$nharvests~eh$startyr)
