# creating equity lines plots ==================================================
getEqlPlot2 <- function(tb, ind = "BTC", strategy = "LS", include_title = F, 
                        legX = 0.1, legY = 0.75, labX = "",
                        modify_y_axis = T,
                        ylim_lower = NA, ylim_upper = NA) {
  
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
  
  p <-
    tb[str_which(names(tb), ind)] %>%
    map(function(x) select(x, -c("signalLS", "signalLO"))) %>%
    map2_dfr(.x = ., 
             .y = names(.) %>% str_split("_") %>% map(tail, 1), 
             .f = function(x, y) mutate(x, sensitivity = y)) %>%
    select(datetime, close, paste0("eql", strategy), sensitivity) %>%
    pivot_wider(values_from = paste0("eql", strategy),
                names_from  = sensitivity) %>%
    pivot_longer(-datetime) %>%
    mutate(name = recode(name, 
                         close = instrument)) %>%
    mutate(name = factor(name)) %>%
    mutate(name = fct_relevel(name, 
                              instrument)) %>%
    ggplot(aes(x = datetime, y = value, col = name)) +
    geom_line() +
    theme_bw() +
    labs(x = labX, y = "") +
    theme(legend.title = element_blank(), 
          # legend.position = "bottom",
          legend.position = c(legX, legY))
  
  if (modify_y_axis) {
    p <- p +
      scale_y_continuous(labels = scales::comma_format(accuracy=1))
  }
  
  if (is.na(ylim_lower) & !is.na(ylim_upper)) {
    p <- p + ylim(NA, ylim_upper)
  }
  
  if (!is.na(ylim_lower) & is.na(ylim_upper)) {
    p <- p + ylim(ylim_lower, NA)
  }
  
  if (!is.na(ylim_lower) & !is.na(ylim_upper)) {
    p <- p + ylim(ylim_lower, ylim_upper)
  }
  
  if(include_title) {
    p <- 
      p +
      labs(
        title = paste0("Equity lines for ", instrument, " strategies"), 
        subtitle = paste0("frequency: ", interval,
                          ", dropout ratio: ", dropout, "%", 
                          ", sequence length: ", sequenceLength,
                          ", trainset length: ", trainSetLength,
                          ", batch size: ", batchSize)
      )
  }
  
  return(p)
}

