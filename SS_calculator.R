library(tidyverse)


# Core variables ----------------------------------------------------------

birth.year <- 1953
claim.age <- 62
inflation.rate <- 1.025

# Indices -----------------------------------------------------------------

# read in the indices
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

# function to fill in NA values with inflation adjusted values
add.inf <- function(vec) {
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

# aoply the inflation function to the AWI, Wage.cap, and bends
indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")] <-
  apply(X = indices.df[, c("AWI", "Wage.cap", "First.bend", "Second.bend")],
        MARGIN = 2, FUN = add.inf) %>%
  as_tibble()

# add inflation whene COLA is NA
indices.df$COLA[is.na(indices.df$COLA)] <- inflation.rate - 1

# add indexed COLA
indices.df$COLA.indexed <- c(1, rep(NA, nrow(indices.df)-1))
for (i in 2:length(indices.df$COLA)){
    indices.df$COLA.indexed[i] <-
      indices.df$COLA.indexed[i - 1] * (1 + indices.df$COLA[i - 1])
}



# Wages -------------------------------------------------------------------

# read in the wage data
wages.df <- read_csv("Wages.csv")

# add year, AWI, wage cap
wages.df$Year <- birth.year + wages.df$Age
wages.df$AWI <- indices.df$AWI[indices.df$Year %in% wages.df$Year]
wages.df$Wage.cap <- indices.df$Wage.cap[indices.df$Year %in% wages.df$Year]

# define the AWI at age 60
AWI.60 <- indices.df$AWI[indices.df$Year == birth.year + 60]

# correct the wages by the cpa and indexing
wages.df$Applicable.wages <- pmin(wages.df$Nominal.wage, wages.df$Wage.cap)
wages.df$Applicable.wages[wages.df$Age >= claim.age] <- 0
wages.df$Indexed.wages <- wages.df$Applicable.wages * AWI.60 / wages.df$AWI

years.working <- min(35, sum(wages.df$Nominal.wage > 0))
highest.35 <- wages.df %>%
  filter(Age < 60) %>%
  top_n(years.working, wt = Indexed.wages) %>%
  pull(Indexed.wages) %>%
  sum()

AIME <- highest.35 / (years.working * 12)

# PIA ---------------------------------------------------------------------


year.62 <- birth.year + 62
first.bend <- indices.df$First.bend[indices.df$Year ==  year.62]
second.bend <- indices.df$Second.bend[indices.df$Year == year.62]

first.bend.wages <- min(AIME, first.bend) * .9
second.bend.wages <- (min(AIME, second.bend) - first.bend) * .32
third.wages <- max(0, AIME - second.bend) * .15

PIA <- sum(c(first.bend.wages, second.bend.wages, third.wages))

# COLA adjustment ---------------------------------------------------------

claim.age.adj <- indices.df[indices.df$Year == birth.year,
                            colnames(indices.df) == paste0("Claim.", claim.age)] %>%
  as.numeric()

monthly.benefit <- PIA * claim.age.adj
annual.benefit <- monthly.benefit * 12

FRA <- indices.df$FRA[indices.df$Year == birth.year]
COLA.FRA <- indices.df$COLA.indexed[indices.df$Year == floor(birth.year + FRA)]

# add in to indexed COLA
wages.df$Indexed.COLA <- indices.df$COLA.indexed[indices.df$Year %in% wages.df$Year] / COLA.FRA

# calculate SS benefit for every age
wages.df$benefit <- (wages.df$Age >= claim.age) * (annual.benefit * wages.df$Indexed.COLA)
