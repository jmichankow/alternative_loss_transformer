optimize_arima_garch_orders <-
  function(x, 
           window_length = 252,
           target = "r") {
    
    first_date <- x$Date[1]
    
    optDates <-
      x %>% 
      mutate(vintageM = substr(Date, 1, 7)) %>%
      #tail(-window_length) %>%
      filter(Date >= (first_date + lubridate::years(1))) %>%
      group_by(vintageM) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      filter(Date == first_date + lubridate::years(1) | 
               substr(Date, 6, 7) %in% c("01", "04", "07", "10")) %>%
      select(Date) %>% 
      pull()
    
    # składnia do optymalizacji
    # n.cores <- 12
    # cl <- makePSOCKcluster(n.cores)
    # stopCluster(cl)
    
    criteria_list <- list()
    nOptDates <- length(optDates)
    # nOptDates <- 10
    for (i in 1:nOptDates) {
      
      criteria <- expand_grid(p = 0:5, q = 0:5, 
                              AIC = NA, 
                              SBC = NA,
                              SHC = NA,
                              HQC = NA)
      
      for(p in 0:5) {
        for(q in 0:5) {
          
          spec <- 
            ugarchspec(
              variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
              mean.model         = list(armaOrder = c(p, q), include.mean = TRUE), 
              distribution.model = "norm")
          
          cat("i =", i, ", p =", p, ", q =", q, "\n")
          # fixed-width window of window_length days
          tmp <- 
            x %>%
            filter(Date <= optDates[i] & Date > (optDates[i] - window_length))
          
          if (target == "r") {
            thisTarget <- tmp$r
          } else {
            thisTarget <- tmp$dClose
          }
         
          
          tryCatch(
            {
              m1 <- ugarchfit(spec = spec, 
                              data = na.omit(thisTarget), 
                              solver = "hybrid",
                              solver.control = list(tol = 1e-7)
                              # solver = "solnp"
                              # solver = “nlminb”
                              # solver = “lbfgs”
                              # solver = “gosolnp”
                              # solver = “nloptr”
              )
              
              criteria[(criteria$p == p & criteria$q == q), "AIC"] <- infocriteria(m1)[1]
              criteria[(criteria$p == p & criteria$q == q), "SBC"] <- infocriteria(m1)[2]
              criteria[(criteria$p == p & criteria$q == q), "SHC"] <- infocriteria(m1)[3]
              criteria[(criteria$p == p & criteria$q == q), "HQC"] <- infocriteria(m1)[4]
            },
            error = function(e) {
              print("error! problem with estimation, skipping this combination of orders!")
              criteria[(criteria$p == p & criteria$q == q), "AIC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "SBC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "SHC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "HQC"] <- NA
            },
            warning = function(e) {
              print("warning! problem with estimation, skipping this combination of orders!")
              criteria[(criteria$p == p & criteria$q == q), "AIC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "SBC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "SHC"] <- NA
              criteria[(criteria$p == p & criteria$q == q), "HQC"] <- NA
            }
          )
           
          
          
        }
      }
      
      criteria_list[[i]] <- criteria
      
    }
    
    names(criteria_list) <- optDates[1:nOptDates]
    return(criteria_list)
    
  }
