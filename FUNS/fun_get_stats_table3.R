# generate tables with performance measures ====================================
get_stats_table3 <- function(tb, ind, add_footnote = NA, 
                             getKbl = T,
                             format = "html",
                             strategies = c("eql_contra",
                                            "eql_moment",
                                            "eql_garch1",
                                            "eql_garch2",
                                            "eql_garch3",
                                            "eql_garch4",
                                            "eql_garch5"),
                             signals = c("signal_contra",
                                         "signal_moment",
                                         "signal_garch1",
                                         "signal_garch2",
                                         "signal_garch3",
                                         "signal_garch4",
                                         "signal_garch5"),
                             scale = 252,
                             ...) {
  
  instrument     <- names(tb)[ind] 
  
  st <-
    map2_df(
      tb[[ind]] %>% select(all_of(strategies)),
      tb[[ind]] %>% select(all_of(signals)),
      ~getPerformanceStats(x = .x, 
                           scale = scale,
                           signals = .y)) %>%
    as.data.frame() %>%
    magrittr::set_rownames(c(strategies %>% gsub("eql_", "", .))) 
  
  result <-
    st %>%
    kbl(
      format = format, 
      row.names = T, digits = 2, booktabs = T, ...
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
