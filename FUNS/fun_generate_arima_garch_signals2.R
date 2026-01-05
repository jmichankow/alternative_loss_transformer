# generate arima signals =======================================================
generate_arima_garch_signals2 <- function(
  dt,
  garch_order = c(1, 1),
  window_length = 252,
  for_name = "fs1",
  sig_name = "signal_arma1",
  model_orders
) {
  
  library(rugarch)

  # merging input data with arma orders
  dt <-
    dt %>% 
    left_join(model_orders %>% 
                rename(Date = optDate) %>%
                rename(arma_p = p) %>%
                rename(arma_q = q), 
              by = "Date") %>%
    mutate(arma_p = zoo::na.locf(arma_p, na.rm = F)) %>%
    mutate(arma_q = zoo::na.locf(arma_q, na.rm = F)) %>%
    mutate(arma_p = ifelse(is.na(arma_p), 1, arma_p)) %>%
    mutate(arma_q = ifelse(is.na(arma_q), 1, arma_q))
    
  # ARIMA_1 - rolling fixed window approach
  dates <- tail(dt$Date, -window_length)
  fs    <- rep(NA, length(dates))
  names(fs) <- dates
  for (i in 1:length(dates)) {
    print(i)
    
    # fixed-width window of window_length days
    tmp <- dt %>% filter(Date < dates[i] & Date > (dates[i] - window_length))
    
    # setting arma_order for given observation
    # arma_order <- c(tmp[1, "arma_p"], tmp[1, "arma_q"])
    arma_order <- c(tmp %>% head(1) %>% select(arma_p) %>% pull(), 
                    tmp %>% head(1) %>% select(arma_q) %>% pull())
    
    # model specification
    spec <- 
      ugarchspec(
        variance.model     = list(model = "sGARCH", garchOrder = garch_order),
        mean.model         = list(armaOrder = arma_order, include.mean = TRUE), 
        distribution.model = "norm")
    
    tryCatch(
      {
        m1 <- ugarchfit(spec = spec, 
                        data = tmp$r %>% na.omit(),
                        solver = "hybrid", 
                        solver.control = list(tol = 1e-7))
        f1 <- ugarchforecast(m1, n.ahead = 1)
      },
      error = function(e) {
        print("error! forecast from previous obs will be used!")
        f1 <- ugarchforecast(m1, n.ahead = 1)
      },
      warning = function(e) {
        print("warning! forecast from previous obs will be used!")
        f1 <- ugarchforecast(m1, n.ahead = 1)
      }
    )
    
    fs[i] <- f1@forecast$seriesFor %>% as.numeric()
  }
  
  # adding forecasts
  dt[, for_name] <- c(rep(NA, window_length), fs)
 
  # defining signals
  dt[, sig_name] <- ifelse(xts::lag.xts(dt[, for_name] %>% pull(), -1) > 0, 1, -1)
  
  return(dt)
}
