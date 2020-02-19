library(tidyverse)
source("R/SS_calculator.R")
source("Plots/ggplot_themes.R")
set.seed(44)

n.sims <- 5000
percentile <- c(0.2, 0.5, 0.8)

# create all combinations of investment returns and claim age
PV.grid <- crossing(Return = seq(1, 1.1, by = 0.001),
                    Claim.age = 62:70)

# calculate PV for all the combinations
sim.PV <- map2_dfr(PV.grid$Return, PV.grid$Claim.age, 
                   function(inv.return, age){
  
  # calculate the benefits
  benefits <- calculate_benefits(birth.year = 1957,
                                 claim.age = age) %>%
  filter(Age >= 62) %>% 
  pull(Benefits)
  
  # simulate the investment returns from 62:100
  sims <- replicate(
    n.sims,
    sim_investment_return(
      cashflows = benefits,
      exp.return = inv.return,
      exp.vol = (inv.return - 1) * 2
    )
  )
  
  # calculate the quantiles for each death age and clean up
  quantiles <- sapply(62:100 - 61, FUN = function(death) {
    quantile(sims[death,], probs = percentile)}) %>%
    t() %>%
    as_tibble() %>%
    mutate(
      death.age = 62:100,
      claim.age = age,
      inv.return = inv.return
    )
})

# rename the percentile columns
names(sim.PV)[1:length(percentile)] <- paste0(percentile * 100, "th percentile of investment returns")

# plot of best claim age by investment return and death age (all claim ages)
sim.PV %>%
  filter(death.age > claim.age) %>% 
  pivot_longer(cols = contains("percentile"), names_to = "Percentile", values_to = "PV") %>% 
  mutate(PV = PV(PV, inflation.rate, death.age - 62)) %>% 
  group_by(inv.return, death.age, Percentile) %>%
  filter(PV == max(PV)) %>%
  ggplot(aes(x = death.age, y = inv.return, fill = as.factor(claim.age))) +
  geom_tile() +
  scale_fill_brewer(name = "Claim age") +
  scale_y_continuous(breaks = seq(1, 1.1, by = 0.01),
                     labels = scales::percent(0:10/100, 1)) +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  labs(title = "Best age to claim Social Security to maximum lifetime benefits",
       subtitle = paste("Based on",
                        scales::comma(n.sims),
                        "simulations per each intersection of return, longevity, and claim age"),
       x = "Expected longevity (years)",
       y = "Expected investment return") +
  facet_wrap(~Percentile, nrow = length(percentile)) +
  light.theme +
  theme(panel.grid.major.y = element_line(color = NA),
        plot.caption = element_text(color = "gray30",
                                    face = 'italic',
                                    size = 7),
        legend.position = "bottom")

ggsave(filename = "Plots/bestClaimVec.png",
       plot = last_plot(),
       device = "png",
       width = 9,
       height = 13)
