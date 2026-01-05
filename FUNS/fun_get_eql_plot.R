# creating equity lines plots ==================================================
get_eql_plot <- function(tb, ind, include_title = F, legX = 0.1, legY = 0.8,
                         labX = "", logPlot = F, accuracy = .000001,
                         strategies = c("eql_contra",
                                        "eql_moment",
                                        "eql_garch1",
                                        "eql_garch2",
                                        "eql_garch3",
                                        "eql_garch4",
                                        "eql_garch5")) {
  
  instrument       <- names(tb)[ind]
  
  p <-
    tb[[ind]] %>%
    rename_all(~str_replace_all(., "eql_", "")) %>%
    select(-starts_with("signal")) %>%
    pivot_longer(cols = c(Close, all_of(strategies %>% gsub("eql_", "", .)))) %>%
    mutate(name = recode(name, 
                         Close = instrument)) %>%
    mutate(name = factor(name)) %>%
    mutate(name = fct_relevel(name, 
                              instrument,
                              strategies %>% gsub("eql_", "", .))) %>%
    ggplot(aes(x = Date, y = value, col = name)) +
    geom_line() +
    theme_bw() +
    labs(x = labX, y = "") +
    theme(legend.title = element_blank(), 
          legend.position = c(legX, legY)
    )
  
  if (include_title) {
    p <- p +
      labs(
        title = paste0("Equity lines for ", instrument, " strategies")
      )
  }
  
  if (logPlot) {
    p <- p + scale_y_continuous(trans = 'log10', labels = scales::comma_format(accuracy = accuracy))
  } else {
    p <- p + scale_y_continuous(labels = scales::comma_format(accuracy = 1))
  }
  
  return(p)
  
}

