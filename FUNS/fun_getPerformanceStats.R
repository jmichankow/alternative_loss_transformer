# set of functions to calculate performance statistics =========================
# aRC
getARC <- function(x, scale = 365){
  nInt <- length(x)
  ARC  <- (x[length(x)] / x[1]) ^ (scale/nInt) - 1
  return(ARC)
}

# aSD
getASD <- function(x, scale = 365){
  r <- xts::diff.xts(x)/xts::lag.xts(x)
  annSdtDev <- sd(r, na.rm = T) * sqrt(scale)
  return(annSdtDev)
}

# MD
getMD <- function(x){
  r <- xts::diff.xts(x)/xts::lag.xts(x)
  return(suppressWarnings(PerformanceAnalytics::maxDrawdown(r)))
}

# IR
getIR <- function(x, scale = 365){
  getARC(x, scale)/getASD(x, scale)
}

# LD
getLD2 <- function(x, scale = 365) {
  currentMax <- x[1]
  LD         <- rep(NA, length(x))
  LD[1]      <- 0
  
  for (i in 2:length(x)) {
    if (x[i] >= currentMax) {
      currentMax <- x[i]
      LD[i]      <- 0
    } else {
      LD[i]      <- LD[i - 1] + 1
    }
  }
  
  LD <- LD / scale
  
  # return(round(LD, 2))
  return(LD)
}

# get all performance stats
getPerformanceStats <- function(x, scale = 365, signals = NULL){
  
  aRC     <- getARC(x, scale)
  aSD     <- getASD(x, scale)
  MD      <- getMD(x)
  MLD     <- max(getLD2(x, scale))
  IR1     <- getIR(x, scale)
  IR2     <- IR1 * aRC * sign(aRC)/ MD
  IR3     <- aRC ^ 3 / (aSD * MD * MLD)
  nObs    <- length(x)
  
  if (is.null(signals)) {
    nTrades <- NA
  } else {
    signals <- ifelse(signals > 0,  1, signals)
    signals <- ifelse(signals < 0, -1, signals)
    nTrades <- signals %>% diff() %>% abs()
    nTrades <- ifelse(nTrades > 0, 1, nTrades)  
    nTrades <- nTrades %>% sum(na.rm = T)
    nTrades <- nTrades + 2
  }
  
  result <- c(100 * aRC, 100 * aSD, 100 * MD, MLD,
              IR1, IR2, IR3, nObs, nTrades)
  names(result) <- c('aRC', "aSD", "MD", "MLD",
                     "IR*", "IR**", "IR***", "nObs", "nTrades")
  return(result)
}
