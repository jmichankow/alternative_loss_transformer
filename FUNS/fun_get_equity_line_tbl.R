# function to created equity line ==============================================
get_equity_line_tbl <- function(dt, 
                                prices_var = "BTC", 
                                signal_var = "signalLS", 
                                normalize_to = NULL,
                                TC = NULL,
                                DFL = 1) {
  
  prices <- dt[, prices_var] %>% pull()
  signal <- dt[, signal_var] %>% pull()
  ret <- (prices / xts::lag.xts(prices)) - 1
  
  # calculating costs
  # costs <- -(abs(signal) * 2 * TC)
  costs <- -(abs(signal - lag(signal)) * 1 * TC) 
  # costs[(signal - lag(signal)) == 0] <- 0
  costs[is.na((signal - lag(signal)))] <- 0
  
  # adding costs
  # eql <- max(-1, ret * as.numeric(lag.xts(signal)) + costs)
  eql <- DFL * ret * as.numeric(lag.xts(signal)) + costs
  # eql[eql < -1] <- -1
  eql[is.na(eql)] <- 0
  eql <- zoo::na.locf(eql)
  eql <- cumprod(1 + eql)
  eql <- eql * prices[1]
  
  if (!is.null(normalize_to)) {
    eql <- eql / prices[1] * normalize_to
  }
  
  eql_var <- signal_var %>% gsub("signal", "eql", .)
  
  dt[, eql_var] <- eql
  
  return(dt)
}
