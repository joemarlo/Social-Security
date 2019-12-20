require(tidyverse)
source("Helper_functions.R")

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


# clean up the indices data.frame -----------------------------------------------------------------

# apply the inflation function to the AWI, Wage.cap, and bends
indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")] <-
  apply(X = indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")],
        MARGIN = 2, FUN = add.inf) %>%
  as_tibble()

# add inflation when COLA is NA
indices.df$COLA[is.na(indices.df$COLA)] <- inflation.rate - 1

# add column that is an index of the COLA (to ease future calculations involving compounded COLA)
indices.df$COLA.indexed <- c(1, rep(NA, nrow(indices.df)-1))
for (i in 2:length(indices.df$COLA)){
    indices.df$COLA.indexed[i] <-
      indices.df$COLA.indexed[i - 1] * (1 + indices.df$COLA[i - 1])
}


# function to calculate benefits ----------------------------------------------------------

calculate.benefits <- function(birth.year, claim.age){
  
  # function calculates the annual Social Security benefits
  # it returns a data frame with benefits along with the associated
  #   years and age
  # the wages used are in the wages.df data frame. These can be modified either within R or
  #  by adjusting the Wages.csv and reloading the data
  
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
# calculate.benefits(birth.year = 1953, claim.age = 62)
# View(lapply(62:70, calculate.benefits, birth.year = 1955))

