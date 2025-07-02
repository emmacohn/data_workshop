library(tidyverse)
library(epiextractr)
library(usethis)
library(skimr)
library(labelled)
library(realtalk)
library(openxlsx2)

# calculate median wage over time: load CPI data from realtalk
cpi_data <- realtalk::c_cpi_u_annual

# set base year to 2024
cpi2024 <- cpi_data$c_cpi_u[cpi_data$year==2024]

#create a data frame with wage information
avg_wage <- load_cps("org", 2000:2024, orgwgt, year, age, emp, wage, selfemp, selfinc) %>%
  filter(age>=16, emp == 1, selfemp !=1, selfinc !=1) %>% 
  # Merge annual CPI data to data frame by year
  left_join(cpi_data, by='year') %>%
  # inflation adjust wages
  mutate(realwage = wage * (cpi2024/c_cpi_u)) %>%
  summarize(wage = MetricsWeighted::weighted_mean(wage, w=orgwgt/12, na.rm=TRUE),
    real_avg_wage = MetricsWeighted::weighted_mean(realwage, w=orgwgt/12, na.rm=TRUE),
            .by=year)

wb <- wb_workbook()

wb$add_worksheet(sheet = "real_wage") $
  add_data(x = avg_wage)

wb_save(wb, "./output/real_annual_wages.xlsx")
