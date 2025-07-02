library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)

# calculate median wage over time: load CPI data from realtalk
cpi_data <- realtalk::c_cpi_u_annual

# set base year to 2024
cpi2024 <- cpi_data$c_cpi_u[cpi_data$year==2024]

#create a data frame with wage information

avg_wage <- load_cps("org", 1979:2024, orgwgt, year, age, lfstat, wage) %>%
  filter(age>=16, lfstat == 1) %>% 
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages
  mutate(realwage = wage * (cpi2024/c_cpi_u)) %>%
  summarize(real_avg_wage = weighted.mean(wage, w=orgwgt/12, na.rm=TRUE),
            .by=year)