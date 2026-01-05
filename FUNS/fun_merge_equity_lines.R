# merging two equity lines with given rebalancing freq =========================
merge_equity_lines <- 
  function(quotes, 
           weightsList, 
           proportionalCost = c(0.002, 0.0001), 
           initialEquity = 1, 
           DFL = 1) {
    # 1 Calculate initial observations of equity lines.
    equityLine <- 
      quotes %>%
      filter(lubridate::as_date(datetime) < names(weightsList)[1] %>% as.Date()) %>%
      select(datetime) %>%
      arrange(datetime) %>%
      distinct() %>%
      mutate(equityLine = initialEquity) %>%
      bind_rows(
        data.frame(datetime = 
                     paste(names(weightsList)[1], "00:00:00") %>%
                     lubridate::ymd_hms(.) %>%
                     lubridate::force_tz("America/New_York"),
                   equityLine = initialEquity * ifelse(weightsList[[1]] %>% sum() > 0,
                                                       (1 - DFL * proportionalCost),
                                                       1)))
    
    # 2 Calculate equity line in consecutive steps.
    for (i in 1:(length(weightsList))) {
      
      # 2.1 set startDate
      startDateTime <- 
        paste(names(weightsList)[i], "00:00:00") %>%
        lubridate::ymd_hms(.) %>%
        lubridate::force_tz("America/New_York")
      startDateTime <- startDateTime + 15*60
      
      # 2.2 set endDate
      if (i < length(weightsList)) {
        endDateTime <- 
          paste(names(weightsList)[i + 1], "00:00:00") %>%
          lubridate::ymd_hms(.) %>%
          lubridate::force_tz("America/New_York")
      } else {
        endDateTime   <- quotes %>% select(datetime) %>% pull() %>% max()
      }
      
      # 2.3 extract currentQuotes
      currentQuotes <- 
        quotes %>%
        filter(datetime >= startDateTime - 15*60 &
                 datetime <= endDateTime &
                 Name %in% names(weightsList[[i]])) %>%
        spread(Name, Price)
      
      # currentQuotes %>% head()
      # currentQuotes %>% tail()
      
      # 2.4 ensure proper order
      currentQuotes <- currentQuotes[c("datetime", names(weightsList[[i]]))]
      
      # 2.5 setting current value of equityLine
      currentEquity <-
        equityLine[equityLine$datetime == startDateTime - 15*60, ]$equityLine
      
      # 2.6 include rebalancing costs
      if (i > 1) {
        # 2.6.1 calculate change of positions
        positionChange <-
          suppressMessages(full_join(((weightsList[[i]] * DFL * as.vector(currentEquity)) /
                                        currentQuotes[1, -1]),
                                     positions))
        
        # 2.6.2 replacing NA with zeros
        positionChange[is.na(positionChange)] <- 0
        
        # 2.6.3 for objects with two rows calculate costs
        if(nrow(positionChange) == 2) {
          
          # calculate abs of position change
          positionChange <- abs(positionChange[1, ] - positionChange[2, ])
          
          # ...
          startQuotes <- 
            quotes %>%
            filter(datetime == startDateTime &
                     Name %in% names(positionChange)) %>%
            spread(Name, Price)
          
          # Ensure proper order and skip dead cryptocurrencies.
          positionChange <- positionChange %>% select(-starts_with(c("date", "Date")))
          startQuotes    <- startQuotes[names(positionChange)]
          
          # Calculate posts based on current prices and positions change.
          costs          <- sum(abs(positionChange) * startQuotes * proportionalCost)
        } else {
          costs <- 0
        }
        
      } else {
        costs <- 0
      }
      
      # 2.7 subtract costs
      currentEquity <- currentEquity - costs
      
      # 2.8 calculate positions
      positions     <- (weightsList[[i]] * DFL * as.vector(currentEquity)) / currentQuotes[1, -1]
      freeBalance   <- ifelse(sum(positions) == 0, currentEquity, currentEquity * (1 - DFL))
      
      # 2.9 calculate equity line
      currentEquityLine <- 
        currentQuotes %>%
        select(datetime) %>%
        mutate(equityLine = 
                 (currentQuotes[, -1] %>% as.matrix()) %*%
                 (positions %>% as.numeric()),
               equityLine = equityLine + freeBalance) 
      
      # 2.10 appending to collected equityLine
      equityLine <- 
        equityLine %>%
        bind_rows(currentEquityLine[-1, ]) %>%
        arrange(datetime) %>%
        mutate(equityLine = equityLine %>% zoo::na.locf())
      # equityLine %>% head()
      # equityLine %>% tail()
    }
    
    # 3 correction of column type
    equityLine <- 
      equityLine %>%
      mutate(eql = .[, 2] %>% select(equityLine) %>% pull() %>% as.numeric()) %>%
      select(datetime, eql)
    
    # 4 returning final object
    return(equityLine)
  }
