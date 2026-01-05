# generate arima signals =======================================================
generate_arima_garch_signals3_sa <- function(
  dt,
  garch_order = c(1, 1),
  window_length = 252,
  for_name = "fs1",
  sig_name = "signal_arma1",
  model_orders,
  target = "r"
) {
  
  library(rugarch)
  library(lubridate)

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
    
  # ARIMA - rolling fixed window approach
  first_date <- dt$Date[1]
  dates <- 
    dt %>%
    filter(Date >= (first_date + lubridate::years(1))) %>%
    select(Date) %>%
    pull()
  # dates <- tail(dt$Date, -365)
  fs    <- rep(NA, length(dates))
  names(fs) <- dates
  prev_arma_order <- c(1, 1)
  
  for (i in 1:length(dates)) {
    print(i)
    
    # fixed-width window of window_length days
    tmp <- dt %>% filter(Date <= dates[i] & Date > (dates[i] - window_length))
    
    # setting arma_order for given observation
    # arma_order <- c(tmp[1, "arma_p"], tmp[1, "arma_q"])
    arma_order <- c(tmp %>% tail(1) %>% select(arma_p) %>% pull(), 
                    tmp %>% tail(1) %>% select(arma_q) %>% pull())
    
    # model specification
    spec <- 
      ugarchspec(
        variance.model     = list(model = "sGARCH", garchOrder = garch_order),
        mean.model         = list(armaOrder = arma_order, include.mean = TRUE), 
        distribution.model = "norm")
    
    # setting target variable
    if (target == "r") {
      thisTarget <- tmp$r
    } else {
      thisTarget <- tmp$dClose
    }
    
    
    # first estimation
    # - no action needed?
    
    # next estimations
    tryCatch(
      {
        if ( (i == 1) ) {
          # print("a!")
          m1 <- ugarchfit(spec = spec, 
                          data = thisTarget %>% na.omit(),
                          solver = "hybrid", 
                          solver.control = list(tol = 1e-7))
          f1 <- ugarchforecast(m1, n.ahead = 1)
          prev_arma_order <- arma_order
        } else if ( i > 1 & !(month(dates[i]) == month(dates[i - 1])) ) {
          # print("b!")
          m1 <- ugarchfit(spec = spec, 
                          data = thisTarget %>% na.omit(),
                          solver = "hybrid", 
                          solver.control = list(tol = 1e-7))
          f1 <- ugarchforecast(m1, n.ahead = 1)
          prev_arma_order <- arma_order
        } else {
          # print("c!")
          spec2 <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = garch_order),
              mean.model         = list(armaOrder = prev_arma_order, include.mean = T), 
              distribution.model = "norm",
              fixed.pars         = as.list(m1@fit$coef))
          f1 <- ugarchforecast(fitORspec = spec2, data = thisTarget %>% na.omit(), n.ahead = 1)
        }
        
      },
      error = function(e) {
        
        if (exists("m1")) {
          print("error1! model from the previous iteration will be used!")
          spec2 <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = garch_order),
              mean.model         = list(armaOrder = prev_arma_order, include.mean = T), 
              distribution.model = "norm",
              fixed.pars         = as.list(m1@fit$coef))
          f1 <- ugarchforecast(fitORspec = spec2, data = thisTarget %>% na.omit(), n.ahead = 1)
        } else {
          print("error2! model (0,0) with the constant will be used!")
          spec <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = garch_order),
              mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE), 
              distribution.model = "norm")
          m1 <- ugarchfit(spec = spec, 
                          data = thisTarget %>% na.omit(),
                          solver = "hybrid", 
                          solver.control = list(tol = 1e-7))
          f1 <- ugarchforecast(m1, n.ahead = 1)
        }
        
      },
      warning = function(e) {
        
        if (exists("m1")) {
          print("warning! model from the previous iteration will be used!")
          spec2 <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = garch_order),
              mean.model         = list(armaOrder = prev_arma_order, include.mean = T), 
              distribution.model = "norm",
              fixed.pars         = as.list(m1@fit$coef))
          f1 <- ugarchforecast(fitORspec = spec2, data = thisTarget %>% na.omit(), n.ahead = 1)
        } else {
          print("warning! model (0,0) with the constant will be used!")
          spec <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = garch_order),
              mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE), 
              distribution.model = "norm")
          m1 <- ugarchfit(spec = spec, 
                          data = thisTarget %>% na.omit(),
                          solver = "hybrid", 
                          solver.control = list(tol = 1e-7))
          f1 <- ugarchforecast(m1, n.ahead = 1)
        }
        
      }
    )
    
    fs[i] <- f1@forecast$seriesFor %>% as.numeric()
    
  }
  
  # adding forecasts
  dt[, for_name] <- c(rep(NA, nrow(dt) - length(fs)), fs)
  
  # defining signals
  # dt[, sig_name] <- ifelse(xts::lag.xts(dt[, for_name] %>% pull(), -1) > 0, 1, -1)
  dt[, sig_name] <- ifelse(dt[, for_name] %>% pull() > 0, 1, -1)
  
  
  return(dt)
}
