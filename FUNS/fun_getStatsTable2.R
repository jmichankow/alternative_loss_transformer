#  generate tables with performance measures ===================================
getStatsTable2 <- function(tb, ind = "BTC", strategy = "LS", add_footnote = NA, 
                           getKbl = T,
                           ...) {
  
  as.numeric2 <- possibly(as.numeric, otherwise = NULL)
  
  instrument     <- ind
  interval       <- 
    names(tb)[str_which(names(tb), ind)] %>% .[1] %>%
    strsplit(., "_") %>% .[[1]] %>% .[2]
  
  dropout <- 
    names(tb)[str_which(names(tb), ind)] %>% 
    strsplit(., "dr") %>% map(2) %>% map(as.numeric2) %>%
    paste(collapse = "/") 
  
  sequenceLength <- 
    names(tb)[str_which(names(tb), ind)] %>% 
    strsplit(., "seq") %>% map(2) %>% map(as.numeric2) %>%
    paste(collapse = "/")
  
  trainSetLength <- 
    names(tb)[str_which(names(tb), ind)] %>% 
    strsplit(., "tr") %>% map(2) %>% map(as.numeric2) %>%
    paste(collapse = "/")
  
  batchSize <- 
    names(tb)[str_which(names(tb), ind)] %>% 
    strsplit(., "bs") %>% map(2) %>% map(as.numeric2) %>%
    paste(collapse = "/")
  
  if (is.na(dropout)) dropout <- "002"
  if (is.na(sequenceLength) & instrument == "BTC")    sequenceLength <- "20"
  if (is.na(sequenceLength) & instrument == "S&P500") sequenceLength <- "14"
  if (is.na(trainSetLength) & instrument == "BTC")    trainSetLength <- "1371"
  if (is.na(trainSetLength) & instrument == "S&P500") trainSetLength <- "948"
  if (is.na(batchSize)      & instrument == "BTC")    batchSize      <- "80"
  if (is.na(batchSize)      & instrument == "S&P500") batchSize      <- "80"
  
  if (dropout == "NULL/NULL/NULL") dropout <- 2
  if (sequenceLength == "NULL/NULL/NULL") sequenceLength <- 20
  if (trainSetLength == "NULL/NULL/NULL") trainSetLength <- 1371
  if (batchSize == "NULL/NULL/NULL") batchSize <- 80
  
  if (dropout == "numeric(0)/numeric(0)/numeric(0)") dropout <- 2
  if (sequenceLength == "numeric(0)/numeric(0)/numeric(0)") sequenceLength <- 20
  if (trainSetLength == "numeric(0)/numeric(0)/numeric(0)") trainSetLength <- 1371
  if (batchSize == "numeric(0)/numeric(0)/numeric(0)") batchSize <- 80
  
  if (!strategy %in% c("LS", "LO")) strategy <- "LS"
  
  scale <- NA
  if (instrument == "BTC" & interval == "1d")     scale = 365
  if (instrument == "BTC" & interval == "1h")     scale = 365 * 24
  if (instrument == "BTC" & interval == "15m")    scale = 365 * 24 * 4
  if (instrument == "S&P500" & interval == "1d")  scale = 252
  if (instrument == "S&P500" & interval == "1h")  scale = 252 * 6.5
  if (instrument == "S&P500" & interval == "15m") scale = 252 * 6.5 * 4
  
  t1 <- 
    tb[str_which(names(tb), ind)] %>%
    map2_dfr(
      .x = ., 
      .y = names(.) %>% str_split("_") %>% map(tail, 1), 
      .f = function(x, y) mutate(x, sensitivity = y)) %>% 
    select(datetime, close, 
           paste0("eql", strategy), paste0("signal", strategy), 
           sensitivity)
  t1a <-
    t1 %>% 
    select(-starts_with("signal")) %>%
    pivot_wider(values_from = paste0("eql", strategy),
                names_from  = c(sensitivity)) %>%
    select(-datetime) 
  
  t1b <-
    t1 %>% 
    select(-starts_with("eql")) %>%
    pivot_wider(values_from = paste0("signal", strategy),
                names_from  = c(sensitivity)) %>%
    select(-datetime) 
  
  st1 <- getPerformanceStats(t1a[, 1] %>% pull(), scale)
  st2 <- getPerformanceStats(t1a[, 2] %>% pull(), scale, t1b[, 2] %>% pull())
  st3 <- getPerformanceStats(t1a[, 3] %>% pull(), scale, t1b[, 3] %>% pull())
  st4 <- getPerformanceStats(t1a[, 4] %>% pull(), scale, t1b[, 4] %>% pull())
  
  st <- 
    bind_rows(st1, st2, st3, st4) %>%
    as.data.frame()
  
  rownames(st) <- c(instrument, 
                    tb[str_which(names(tb), ind)] %>% 
                      names() %>%
                      str_split("_") %>%
                      map_chr(tail, 1))
  result <- 
    st %>%
    kbl(format = "latex", 
        row.names = T, 
        # digits = 2, 
        digits = c(2, 2, 2, 2, 2, 3, 3, 0, 0), 
        booktabs = T,
        ...) %>%
    kable_styling(font_size = 8, 
                  latex_options = "hold_position")
  
  if(!is.na(add_footnote)) {
    result <-
      result %>%
      add_footnote(add_footnote, threeparttable = T)
  }
  
  if (getKbl) return(result)
  else return(st)
  
}
