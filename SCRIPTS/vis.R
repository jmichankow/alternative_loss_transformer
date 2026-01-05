library(tidyverse)
library(feather)

# 1 ============================================================================
filenames <- dir("EXPORTS", full.names = T)
dt <- 
  filenames |> 
  map(~read_feather(.)) |> 
  setNames(
    filenames |> 
    str_replace_all("_madl", "") |> 
    str_replace_all("EXPORTS/", "") |> 
    str_replace_all("train1y_lo.feather", "") |> 
    str_sub(1, -2)
)

dt |> map_df(tail, 1)
dt |> map_df(head, 1)

dt |> 
  data.table::rbindlist(idcol = "ticker") |> 
  as_tibble() |>
  filter(ti)
  group_by(ticker) |> 
  mutate(obs = row_number()) |> 
  ungroup() |> 
  ggplot(aes(obs, eql_bh, col = ticker)) +
  geom_line()

/Users/pawel/REPOS/jmichankow/alternative_loss_transformer/EXPORTS/spx_madl_




lstm %>% glimpse()
lstm[[1]] %>% glimpse()
lstm[[1]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[1]]$signal_short %>% table(useNA = "ifany")

lstm[[2]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[2]]$signal_short %>% table(useNA = "ifany")

lstm[[3]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[3]]$signal_short %>% table(useNA = "ifany")

lstm[[4]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[4]]$signal_short %>% table(useNA = "ifany")

lstm[[5]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[5]]$signal_short %>% table(useNA = "ifany")

lstm[[6]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[6]]$signal_short %>% table(useNA = "ifany")

lstm[[1]] %>% 
  select(Date, eql_ls, signal) %>% 
  rename(signal_lstm = signal, eql_lstm = eql_ls)
  
# 2 ============================================================================
filenames <- dir("DATA/lstm/SEQ_LENGTH", full.names = T)
lstm <- 
  filenames %>%
  map(~read_feather(.)) %>%
  setNames(paste0(filenames %>% substr(., 27, 29), "_", filenames %>% substr(., 36, 39))
             )

lstm %>% map_df(tail, 1)
lstm %>% map_df(head, 1)

lstm %>%
  data.table::rbindlist(idcol = "ticker") %>%
  as_tibble() %>%
  group_by(ticker) %>%
  mutate(obs = row_number()) %>%
  ungroup() %>%
  ggplot(aes(obs, eql_bh, col = ticker)) +
  geom_line()

# do wyników uwzględniamy: signal, eql_ls

lstm %>% glimpse()
lstm[[1]] %>% glimpse()
lstm[[1]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[1]]$signal_short %>% table(useNA = "ifany")

lstm[[2]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[2]]$signal_short %>% table(useNA = "ifany")

lstm[[3]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[3]]$signal_short %>% table(useNA = "ifany")

lstm[[4]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[4]]$signal_short %>% table(useNA = "ifany")

lstm[[5]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[5]]$signal_short %>% table(useNA = "ifany")

lstm[[6]] %>% select(signal, signal_long) %>% table(useNA = "ifany")
lstm[[6]]$signal_short %>% table(useNA = "ifany")

lstm[[1]] %>% 
  select(Date, eql_ls, signal) %>% 
  rename(signal_lstm = signal, eql_lstm = eql_ls)
