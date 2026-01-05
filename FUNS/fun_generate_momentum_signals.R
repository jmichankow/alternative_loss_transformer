generate_momentum_signals <- function(dt) {
  dt <-
    dt %>%
    mutate(signal_moment = ifelse(r > 0, 1, -1))
}