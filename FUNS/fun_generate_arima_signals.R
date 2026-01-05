# generate arima signals =======================================================
generate_arima_signals <- function(
  dt,
  order = c(1, 0, 1),
  window_length = 252,
  for_name = "fs1",
  sig_name = "signal_arma1"
) {
  
  # ARIMA_1 - rolling fixed window approach
  dates <- tail(dt$Date, -window_length)
  fs    <- rep(NA, length(dates))
  names(fs) <- dates
  for (i in 1:length(dates)) {
    # fixed-width window of window_length days
    tmp <- dt %>% filter(Date < dates[i] & Date > (dates[i] - window_length))
    # tmp$Date %>% summary()
    m1 <- Arima(tmp$r, order = order, method = "ML")
    f1 <- forecast(m1, h = 1)
    print(i)
    fs[i] <- f1$mean %>% as.numeric()
  }
  
  # adding forecasts
  dt[, for_name] <- c(rep(NA, window_length), fs)
 
  # defining signals
  dt[, sig_name] <- ifelse(xts::lag.xts(dt[, for_name] %>% pull(), -1) > 0, 1, -1)
  
  return(dt)
}
