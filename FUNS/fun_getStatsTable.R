# generate tables with performance measures ====================================
getStatsTable <- function(tb, ind, add_footnote = NA, 
                          getKbl = T,
                          format = "html",
                          ...) {
  
  instrument     <- names(tb)[ind] 
  
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
    if (is.na(batchSize)      & instrument == "BTC")    batchSize      <- "80"
    if (is.na(batchSize)      & instrument == "S&P500") batchSize      <- "80"
  }
  
  scale <- NA
  if (instrument == "btc")     scale = 365
  if (instrument == "gld")     scale = 252
  if (instrument == "spx")     scale = 252
  if (instrument == "ung")     scale = 252
  if (instrument == "uso")     scale = 252
  if (instrument == "zwf")     scale = 252
  
  if (0) {
    if (instrument == "BTC" & interval == "1h")     scale = 365 * 24
    if (instrument == "BTC" & interval == "15m")    scale = 365 * 24 * 4
    if (instrument == "S&P500" & interval == "1d")  scale = 252
    if (instrument == "S&P500" & interval == "1h")  scale = 252 * 6.5
    if (instrument == "S&P500" & interval == "15m") scale = 252 * 6.5 * 4
    if (instrument == "BTC"    & interval %in% c("combo", "combo2")) scale = 365 *  24 * 4
    if (instrument == "S&P500" & interval %in% c("combo", "combo2")) scale = 252 * 6.5 * 4
  }
  
  st1 <- getPerformanceStats(tb[[ind]]$Close, scale)
  # st2 <- getPerformanceStats(tb[[ind]]$eql_arima1, scale, tb[[ind]]$signal_arima1)
  st2 <- getPerformanceStats(tb[[ind]]$eql_garch1, scale, tb[[ind]]$signal_garch1)
  # st3 <- getPerformanceStats(tb[[ind]]$eql_arima2, scale, tb[[ind]]$signal_arima2)
  st3 <- getPerformanceStats(tb[[ind]]$eql_garch2, scale, tb[[ind]]$signal_garch2)
  st4 <- getPerformanceStats(tb[[ind]]$eql_moment, scale, tb[[ind]]$signal_moment)
  st5 <- getPerformanceStats(tb[[ind]]$eql_contra, scale, tb[[ind]]$signal_contra)
  
  st <- 
    bind_rows(st1, st2, st3, st4, st5) %>%
    as.data.frame()
  # rownames(st) <- c(instrument, "ARIMA1", "ARIMA2", "MOMENT", "CONTRA")
  rownames(st) <- c(instrument, "GARCH1", "GARCH2", "MOMENT", "CONTRA")
  
  result <-
    st %>%
    kbl(
      format = format, 
      row.names = T, digits = 2, booktabs = T,
      ...
    ) %>%
    kable_styling(font_size = 14, 
                  latex_options = "hold_position")
  
  if(!is.na(add_footnote)) {
    result <-
      result %>%
      add_footnote(add_footnote, threeparttable = T)
  }
  
  if (getKbl) return(result)
  else return(st)
}
