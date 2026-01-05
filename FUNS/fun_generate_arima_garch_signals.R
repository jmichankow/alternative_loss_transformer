# generate arima signals =======================================================
generate_arima_garch_signals <- function(
  dt,
  armaOrder = c(1, 1),
  garchOrder = c(1, 1),
  window_length = 252,
  for_name = "fs1",
  sig_name = "signal_arma1"
) {
  
  library(rugarch)
  
  # model specification
  spec <- 
    ugarchspec(
      variance.model     = list(model = "sGARCH", garchOrder = garchOrder),
      mean.model         = list(armaOrder = armaOrder, include.mean = TRUE), 
      distribution.model = "norm")
  
  
  # ARIMA_1 - rolling fixed window approach
  dates <- tail(dt$Date, -window_length)
  fs    <- rep(NA, length(dates))
  names(fs) <- dates
  for (i in 1:length(dates)) {
    print(i)
    # fixed-width window of window_length days
    tmp <- dt %>% filter(Date < dates[i] & Date > (dates[i] - window_length))
    m1 <- ugarchfit(spec = spec, data = tmp$r %>% na.omit(),
                    solver = "hybrid")
    f1 <- ugarchforecast(m1, n.ahead = 1)
    fs[i] <- f1@forecast$seriesFor %>% as.numeric()
  }
  
  # adding forecasts
  dt[, for_name] <- c(rep(NA, window_length), fs)
 
  # defining signals
  dt[, sig_name] <- ifelse(xts::lag.xts(dt[, for_name] %>% pull(), -1) > 0, 1, -1)
  
  return(dt)
}
