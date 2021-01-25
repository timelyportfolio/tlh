# see if we can evaluate after-tax performance of a simple moving average system
# improvement ideas:
#    1) functionalize
#    2) optimize
#    3) separate out system portfolio accounting and after-tax evaluation routine
#    4) test code and test parameters


library(quantmod)

getSymbols("^GSPC", from = "1900-01-01")

# signal rolling period parameter
period <- 12
# portfolio value parameter
portval <- 100000
# short term capital gains rate
st_rate <- 0.4
# long term capital gains rate
lt_rate <- 0.2

monthly <- to.monthly(GSPC)
monthly <- merge(
  monthly[,6],
  monthlyReturn(monthly[,6]),
  rollmeanr(x=monthly[,6],k=period)
)
names(monthly) <- c("price","ret","movavg")
monthly$signal <- lag(monthly$price > monthly$movavg)

monthly$portval <- portval
monthly$buyhold <- portval
monthly$system <- portval
monthly$system_posttax <- portval
monthly$gl <- 0
monthly$basis <- 0
monthly$holdperiod <- 0
monthly$carryforward_short <- 0
monthly$carryforward_long <- 0
monthly$yeargl_short <- 0
monthly$yeargl_long <- 0
monthly$tax <- 0

monthly <- cbind(
  date = as.Date(index(monthly)),
  as.data.frame(monthly),
  entry = as.Date(index(monthly))
)

# evaluate the system manually so we can try to account for taxes
for(i in seq_len(nrow(monthly))) {
  if(!is.na(monthly[i,"signal"])) {
    monthly[i,"buyhold"] <- monthly[i-1,"buyhold"] * (1 + monthly[i,"ret"])
    monthly[i,"system"] <- monthly[i-1,"system"] * (1 + monthly[i,"signal"] * monthly[i,"ret"])
    monthly[i,"system_posttax"] <- monthly[i-1,"system_posttax"] * (1 + monthly[i,"signal"] * monthly[i,"ret"])
    if(monthly[i,"signal"] == 1 && (monthly[i-1,"signal"] == 0 || is.na(monthly[i-1,"signal"]))) {
      monthly[i,"entry"] <- monthly[i-1,"date"]
      monthly[i,"basis"] <- monthly[i-1,"system"]
    } else if((!is.na(monthly[i,"signal"])) && monthly[i,"signal"] == 1) {
      monthly[i,"entry"] <- monthly[i-1, "entry"]
      monthly[i,"basis"] <- monthly[i-1, "basis"]
    }
    # evaluate tax consequences
    if(monthly[i,"signal"] == 0 && (!is.na(monthly[i-1,"signal"])) && monthly[i-1,"signal"] == 1) {
      monthly[i,"entry"] <- monthly[i-1, "entry"]
      monthly[i,"basis"] <- monthly[i-1, "basis"]
      monthly[i,"gl"] <- monthly[i,"system"] - monthly[i,"basis"]
      monthly[i,"holdperiod"] <- difftime(monthly[i,"date"],monthly[i,"entry"],units="days")
    }
  }
  # calculate gain/loss
  if(i > 1) {
    if(monthly[i,"holdperiod"] >= 365) {
      monthly[i,"yeargl_long"] <- monthly[i-1,"yeargl_long"] + monthly[i,"gl"]
      monthly[i,"yeargl_short"] <- monthly[i-1,"yeargl_short"]
    }
    if(monthly[i,"holdperiod"] < 365 && monthly[i,"holdperiod"] > 0) {
      monthly[i,"yeargl_short"] <- monthly[i-1,"yeargl_short"] + monthly[i,"gl"]
      monthly[i,"yeargl_long"] <- monthly[i-1,"yeargl_long"]
    }
    if(monthly[i,"holdperiod"] == 0) {
      monthly[i,"yeargl_long"] <- monthly[i-1,"yeargl_long"]
      monthly[i,"yeargl_short"] <- monthly[i-1,"yeargl_short"]
    }
    # evaluate tax consequences at the end of each year
    if(format(monthly[i,"date"],"%m") == "12") {
      #browser()
      carryforward_long <- monthly[[i-1,"carryforward_long",exact=TRUE]]
      carryforward_short <- monthly[[i-1,"carryforward_short",exact=TRUE]]
      yeargl_long <- monthly[i,"yeargl_long"]
      yeargl_short <- monthly[i,"yeargl_short"]
      yeargl_long_o <- yeargl_long
      yeargl_short_o <- yeargl_short
      # first offset yearly short term gains with carryforward short term losses
      if(carryforward_short <= 0) {
        if(yeargl_short > 0) {
          yeargl_short <- yeargl_short + carryforward_short
          carryforward_short <- yeargl_short_o + carryforward_short
        } else {
          carryforward_short <- yeargl_short_o + carryforward_short
        }
        if(carryforward_short > 0) {carryforward_short <- 0}
      }
      # offset yearly long term gains with carryforward long term losses
      if(carryforward_long <= 0) {
        if(yeargl_long > 0) {
          yeargl_long <- yeargl_long + carryforward_long
          carryforward_long <- yeargl_long_o + carryforward_long
        } else {
          carryforward_long <- yeargl_long_o + carryforward_long
        }
        if(carryforward_long > 0) {carryforward_long <- 0}
      }
      # if carryforward short still available then offset long losses
      if(carryforward_short < 0 && yeargl_long > 0) {
        yeargl_long_o <- yeargl_long
        yeargl_long <- yeargl_long + carryforward_short
        carryforward_short <- yeargl_long_o + carryforward_short
        if(carryforward_short > 0) {carryforward_short <- 0}
      }
      # if carryforward long still available then offset short losses
      if(carryforward_long < 0 && yeargl_short > 0) {
        yeargl_short_o <- yeargl_short
        yeargl_short <- yeargl_short + carryforward_long
        carryforward_long <- yeargl_short_o + carryforward_long
        if(carryforward_long > 0) {carryforward_long <- 0}
      }
      # now that short and long carryforwards fully applied calculate tax
      tax <- 0
      if(yeargl_short > 0) {
        tax <- tax - yeargl_short * st_rate
      }
      if(yeargl_long > 0) {
        tax <- tax - yeargl_long * lt_rate
      }
      monthly[i,"tax"] <- tax
      monthly[i,"carryforward_short"] <- carryforward_short
      monthly[i,"carryforward_long"] <-  carryforward_long
      # subtract tax from system post tax value
      monthly[i,"system_posttax"] <- monthly[i,"system_posttax"] + tax
      # reset yearly gl acccumulators
      monthly[i,"yeargl_long"] <- 0
      monthly[i,"yeargl_short"] <- 0
    } else {
      monthly[i,"carryforward_short"] <- monthly[[i-1,"carryforward_short",exact=TRUE]]
      monthly[i,"carryforward_long"] <- monthly[[i-1,"carryforward_long",exact=TRUE]]
    }
  }
}

# see total carryforward divided by system portfolio value
plot((monthly$carryforward_short + monthly$carryforward_long)/monthly$system, type="l")

# convert portfolio values to xts
val_xts <- as.xts(monthly[,c("buyhold","system","system_posttax")], order.by = as.Date(monthly$date))
chartSeries(val_xts$buyhold, yrange = range(val_xts), log=TRUE, name="Cumulative Value")
addTA(val_xts$system, on=1)
addTA(val_xts$system_posttax, on=1)

# plot cumulative taxes
plot(cumsum(monthly$tax), type="l")
