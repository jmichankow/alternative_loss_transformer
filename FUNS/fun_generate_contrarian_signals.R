generate_contrarian_signals <- function(dt) {
  dt <-
    dt %>%
    mutate(signal_contra = ifelse(r < 0, 1, -1))
}


