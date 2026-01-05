# function to created equity line ==============================================
get_equity_line <- function(dt, 
                            prices_var = "close", 
                            signal_var = "signalLS", 
                            normalize_to = NULL, 
                            TC = NULL,
                            DFL = 1) {
  prices <- dt[, prices_var] 
  signal <- dt[, signal_var] 
  ret <- (prices / xts::lag.xts(prices)) - 1
  names(ret) <- "ret"
  
  # calculating costs
  # costs <- -(abs(signal) * 2 * TC)
  costs <- -(abs(signal - xts::lag.xts(signal)) * 1 * TC) 
  # costs[(signal - xts::lag.xts(signal)) == 0] <- 0
  costs[is.na((signal - xts::lag.xts(signal)))] <- 0
  
  # adding costs
  # eql <- max(-1, ret * as.numeric(lag.xts(signal)) + costs)
  eql <- DFL * ret * as.numeric(lag.xts(signal)) + costs
  eql[eql < -1] <- -1
  names(eql) <- "eql"
  eql[is.na(eql), ] <- 0
  eql <- zoo::na.locf(eql)
  eql <- cumprod(1 + eql)
  
  result <- merge(dt[, c(prices_var, signal_var)], eql) 
  result[, 3] <- result[, 3] * as.numeric(result[1, 1])
  
  if (!is.null(normalize_to)) {
    result[, 3] <- result[, 3] / as.numeric(result[1, 1]) * normalize_to
  }
  
  return(result)
}
