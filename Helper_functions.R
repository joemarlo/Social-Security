require(tidyverse)


# helper functions --------------------------------------------------------

add.inf <- function(vec) {
  # function to fill in NA values with inflation adjusted values
  # returns a vector containing the original vector's values
  #  with the NAs filled in
  
  # add inflation for values that have a preceding value
  for (i in seq_along(vec)) {
    if (all(is.na(vec[i]), !is.na(vec[i-1]), length(!is.na(vec[i-1])) == 1)) {
      vec[i] <- vec[i - 1] * inflation.rate      
    }
  }
  # remove inflation for values that don't have preceding value
  for (i in rev(seq_along(vec))){
    if (is.na(vec[i]) & !is.na(vec[i+1])) {
      vec[i] <- vec[i + 1] / inflation.rate      
    }
  }
  return(vec)
}

add.investment.return <- function(cashflows, rate) {
  # add investment rate to a series of cashflows
  # returns a vector of the new investment account balances by period
  inv <- c()
  inv[1] <- cashflows[1]
  for (i in 2:length(cashflows)) {
    inv[i] <- (inv[i - 1] * rate) + cashflows[i]
  }
  return(inv)
}

sim.investment.return <- function(cashflows, mean, sd) {
  # simulates investment returns for a series of cashflows
  # returns a vector of the new investment account balances by period
  inv <- c()
  inv[1] <- cashflows[1]
  rates <- rnorm(length(cashflows)-1, mean = mean, sd = sd)
  for (i in 2:length(cashflows)) {
    inv[i] <- (inv[i - 1] * rates[i-1]) + cashflows[i]
  }
  return(inv)
}

grab.percentile.inv.return <- function(cashflows, mean, sd, n.sims, percentile) {
  # simulates investment returns for a series of cashflows
  # returns a single quantile of the ending values
  end.values <- replicate(n.sims,
                          last(
                            sim.investment.return(
                              cashflows = cashflows, mean = mean, sd = sd
                            )))
  qle <- as.numeric(quantile(end.values, probs = percentile))
  return(qle)
}

NPV <- function(cashflows, rate){
  # calculate the net present value of a series of cash flows
  # returns a single value
  present.values <- sapply(seq_along(cashflows), function(i){
    cashflows[i] / rate^(i-1)
  })
  
  NPV <- sum(c(cashflows[1], present.values[2:length(present.values)]))
  return(NPV)
}

PV <- function(value, rate, periods){
  # calculates the present value by discounting
  value / (rate^(periods))
}
