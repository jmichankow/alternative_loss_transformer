# transform xts objects to tibbles ==========================
xts2tbl <- function(x, closeVar = "BTC") {
  
  ind <- index(x)
  
  output <-
    x %>%
    as.data.frame(x) %>%
    as_tibble() %>%
    mutate(datetime = ind) %>%
    rename(
      # !!closeVar := close,
      signalLS = signal, 
      eqlLS = eql
    ) %>%
    select(datetime, everything())
  
  return(output)
}
