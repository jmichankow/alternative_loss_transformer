get_optimum_orders <- function(y, criterion = "AIC") {
  
  if (criterion == "AIC") {
    y %>%
      map(function(x) x %>% arrange(AIC) %>% select(p, q, AIC) %>% head(1)) %>%
      bind_rows() %>%
      mutate(optDate = names(y) %>% as.Date()) %>%
      select(optDate, everything())
  } else if (criterion == "SBC") {
    y %>%
      map(function(x) x %>% arrange(SBC) %>% select(p, q, SBC) %>% head(1)) %>%
      bind_rows() %>%
      mutate(optDate = names(y) %>% as.Date()) %>%
      select(optDate, everything())
  } else if (criterion == "SHC") {
    y %>%
      map(function(x) x %>% arrange(SHC) %>% select(p, q, SHC) %>% head(1)) %>%
      bind_rows() %>%
      mutate(optDate = names(y) %>% as.Date()) %>%
      select(optDate, everything())
  } else if (criterion == "HQC") {
    y %>%
      map(function(x) x %>% arrange(HQC) %>% select(p, q, HQC) %>% head(1)) %>%
      bind_rows() %>%
      mutate(optDate = names(y) %>% as.Date()) %>%
      select(optDate, everything())
  }
  
}

