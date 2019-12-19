library(tidyverse)

# next steps
# double check assumptions for projecting indices
# convert benefits calculation from annual to monthly

# read in the wage data
wages.df <- read_csv("Wages.csv")

# read in the indices data
indices.df <-
  read_csv(
    "Indices.csv",
    col_types = cols(
      col_number(),
      COLA = col_double(),
      First.bend = col_number(),
      Second.bend = col_number(),
      Wage.cap = col_number(),
      Year = col_number()
    )
  )

# set the default inflation rate  
inflation.rate <- 1.025


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

add.investment.return <- function(vec, rate) {
  # add investment rate to a series of cashflows
  # returns a vector of the new investment account balances by period
  inv <- c()
  inv[1] <- vec[1]
  for (i in 2:length(vec)) {
    inv[i] <- (inv[i - 1] * rate) + vec[i]
  }
  return(inv)
}

NPV <- function(vec, rate){
  # calculate the net present value of a series of cash flows
  # returns a single value
  present.values <- sapply(seq_along(vec), function(i){
    vec[i] / rate^i
  })
  
  NPV <- sum(c(vec[1], present.values[2:length(present.values)]))
  return(NPV)
}

# clean up the indices data.frame -----------------------------------------------------------------

# apply the inflation function to the AWI, Wage.cap, and bends
indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")] <-
  apply(X = indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")],
        MARGIN = 2, FUN = add.inf) %>%
  as_tibble()

# add inflation when COLA is NA
indices.df$COLA[is.na(indices.df$COLA)] <- inflation.rate - 1

# add indexed COLA
indices.df$COLA.indexed <- c(1, rep(NA, nrow(indices.df)-1))
for (i in 2:length(indices.df$COLA)){
    indices.df$COLA.indexed[i] <-
      indices.df$COLA.indexed[i - 1] * (1 + indices.df$COLA[i - 1])
}


# function to calculate benefits ----------------------------------------------------------

calculate.benefits <- function(birth.year, claim.age){

  # index the wages ---------------------------------------------------------

  # add year, AWI, wage cap
  wages.df$Year <- birth.year + wages.df$Age
  wages.df$AWI <- indices.df$AWI[indices.df$Year %in% wages.df$Year]
  wages.df$Wage.cap <- indices.df$Wage.cap[indices.df$Year %in% wages.df$Year]
  
  # define the AWI at age 60
  AWI.60 <- indices.df$AWI[indices.df$Year == birth.year + 60]
  
  # correct the wages by the wage cap
  wages.df$Applicable.wages <- pmin(wages.df$Nominal.wage, wages.df$Wage.cap)
  
  # $0 wages after claim age
  wages.df$Applicable.wages[wages.df$Age >= claim.age] <- 0
  
  # adjust wages by indexation by AWI
  wages.df$Indexed.wages <- wages.df$Applicable.wages * AWI.60 / wages.df$AWI
  
  # caclulate the number of years the person worked, capped at 35
  years.working <- min(35, sum(wages.df$Nominal.wage > 0))
  
  # sum the top earning years
  highest.35 <- wages.df %>%
    filter(Age < 60) %>%
    top_n(years.working, wt = Indexed.wages) %>%
    pull(Indexed.wages) %>%
    sum()
  
  # caclulate the Average Indexed Monthly Earnings
  AIME <- highest.35 / (years.working * 12)
  
  # PIA ---------------------------------------------------------------------
  
  year.62 <- birth.year + 62
  first.bend <- indices.df$First.bend[indices.df$Year ==  year.62]
  second.bend <- indices.df$Second.bend[indices.df$Year == year.62]
  
  # calculate the earnings per bend point
  first.bend.wages <- min(AIME, first.bend) * .9
  second.bend.wages <- (min(AIME, second.bend) - first.bend) * .32
  third.wages <- max(0, AIME - second.bend) * .15
  
  # sum to get the PIA
  PIA <- sum(c(first.bend.wages, second.bend.wages, third.wages))
  
  # COLA adjustment ---------------------------------------------------------
  
  # find the value to adjust the PIA by for early/late claiming
  claim.age.adj <- indices.df[indices.df$Year == birth.year,
                              colnames(indices.df) == paste0("Claim.", claim.age)] %>%
    as.numeric()
  
  # adjust the benefit by this amount
  monthly.benefit <- PIA * claim.age.adj
  annual.benefit <- monthly.benefit * 12
  
  # find the COLA index at FRA (this could be wrong; might be at claim age)
  FRA <- indices.df$FRA[indices.df$Year == birth.year]
  COLA.FRA <- indices.df$COLA.indexed[indices.df$Year == floor(birth.year + FRA)]
  
  # add in to indexed COLA
  wages.df$Indexed.COLA <- indices.df$COLA.indexed[indices.df$Year %in% wages.df$Year] / COLA.FRA
  
  
  # benefits ----------------------------------------------------------------
  
  # create data.frame of year, age
  benefits.df <- wages.df[, c("Year", "Age")] 
  # calculate SS benefit for every age
  benefits.df$Benefits <- (benefits.df$Age >= claim.age) * (annual.benefit * wages.df$Indexed.COLA)
  
  return(benefits.df)
}

# test the function
calculate.benefits(birth.year = 1953, claim.age = 62)

# View(lapply(62:70, calculate.benefits, birth.year = 1955))

# discount the benefits ---------------------------------------------------

# calculate NPV of one series of benefits, starting at age 62
calculate.benefits(birth.year = 1957, claim.age = 62) %>%
  filter(Age >= 62) %>%
  pull(Benefits) %>%
  add.investment.return(., rate = 1.05) %>%
  NPV(., rate = inflation.rate)

# create all combinations of investment returns and claim age
NPV.grid <- expand.grid(Return = seq(1, 1.1, by = 0.001),
                        Claim.age = c(62, 66, 70),
                        Death.age = 63:100) %>% as_tibble()

# calculate NPV for all the combinations
NPV.grid$NPV <- pmap(list(NPV.grid$Return, NPV.grid$Claim.age, NPV.grid$Death.age),
                     function(return, age, death){
  
  # calculate benefits
  benefits <- calculate.benefits(birth.year = 1957, claim.age = age)
  benefits <- benefits[benefits$Age >= 62 & benefits$Age <= death,]$Benefits
  
  # add investment return then calculate NPV
  investment.value <- add.investment.return(benefits, return)
  NPV.value <- NPV(investment.value, inflation.rate)
  
  return(NPV.value)
}) %>% unlist()


# plot of best claim age by investment return and death age
NPV.grid %>%
  group_by(Return, Death.age) %>%
  filter(NPV == max(NPV)) %>%
  ggplot(aes(x = Death.age, y = Return, fill = as.factor(Claim.age))) +
  geom_tile() +
  scale_fill_brewer(name = "Claim age") +
  scale_y_continuous(breaks = seq(1, 1.1, by = 0.01),
                     labels = scales::percent(0:10/100, 1)) +
  labs(title = "Best claim age for maximum lifetime benefits",
       subtitle = "Assuming benefits are invested",
       x = "Longevity (years)",
       y = "Investment return") +
  theme_minimal() +
  theme(panel.grid = element_line(color = NA))

ggsave(filename = "Plots/bestClaim.png",
       plot = last_plot(),
       device = "png",
       width = 6,
       height = 4)

