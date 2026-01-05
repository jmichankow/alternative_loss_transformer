# creating equity lines plots ==================================================
getEqlPlot <- function(tb, ind, include_title = F, legX = 0.1, legY = 0.8,
                       labX = "", logPlot = F) {
  
  instrument       <- names(tb)[ind]
  
  if (0) {
    instrument     <- names(tb)[ind] %>% strsplit(., "_")    %>% unlist %>% .[1]
    interval       <- names(tb)[ind] %>% strsplit(., "_")    %>% unlist %>% .[2]
    dropout        <- names(tb)[ind] %>% strsplit(., "_dr")  %>% unlist %>% .[2]
    sequenceLength <- names(tb)[ind] %>% strsplit(., "_seq") %>% unlist %>% .[2]
    trainSetLength <- names(tb)[ind] %>% strsplit(., "_tr")  %>% unlist %>% .[2]
    batchSize      <- names(tb)[ind] %>% strsplit(., "_bs")  %>% unlist %>% .[2]
  }
  
  if (0) {
    if (is.na(dropout)) dropout <- "002"
    if (is.na(sequenceLength) & instrument == "BTC")    sequenceLength <- "20"
    if (is.na(sequenceLength) & instrument == "S&P500") sequenceLength <- "14"
    if (is.na(trainSetLength) & instrument == "BTC")    trainSetLength <- "1371"
    if (is.na(trainSetLength) & instrument == "S&P500") trainSetLength <- "948"
    if (is.na(batchSize) & instrument == "BTC")         batchSize <- "80"
    if (is.na(batchSize) & instrument == "S&P500")      batchSize <- "80"
  }
  
  
  p <-
    tb[[ind]] %>%
    select(-starts_with("signal")) %>%
    pivot_longer(cols = c(Close, starts_with("eql_"))) %>%
    mutate(name = recode(name, 
                         Close = instrument, 
                         # eql_arima1 = "ARIMA1",
                         # eql_arima2 = "ARIMA2", 
                         eql_garch1 = "GARCH1",
                         eql_garch2 = "GARCH2", 
                         eql_moment = "MOMENT",
                         eql_contra = "CONTRA", 
    )) %>%
    mutate(name = factor(name)) %>%
    mutate(name = fct_relevel(name, 
                              instrument,
                              # "ARIMA1",
                              # "ARIMA2",
                              "GARCH1",
                              "GARCH2",
                              "MOMENT",
                              "CONTRA")) %>%
    ggplot(aes(x = Date, y = value, col = name)) +
    geom_line() +
    theme_bw() +
    labs(x = labX, y = "") +
    theme(legend.title = element_blank(), 
          # legend.position = "bottom"
          legend.position = c(legX, legY)
    )
  
  if (include_title) {
    p <- p +
      labs(
        title = paste0("Equity lines for ", instrument, " strategies")
      )
  }
  
  if (logPlot) {
    p <- p + scale_y_continuous(trans = 'log10', labels = scales::comma_format(accuracy = .000001))
  } else {
    p <- p + scale_y_continuous(labels = scales::comma_format(accuracy = 1))
  }
  
  return(p)
  
}

