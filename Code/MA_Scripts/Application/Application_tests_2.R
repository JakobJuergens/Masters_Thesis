# Load necessary packages
library(tidyverse)
library(stringr)
library(lubridate)

# set parameters
approxQ <- 500
n_func <- 4000

# read in data objects
cleaned_data <- readRDS('../../Data/cleaned_electricity_demand_fd.RDS')
reg_tibble <- readRDS('../../Data/electricity_demand_reg_tibble_holiday_cleaned.RDS')

# extract coefficient objects for weekdays and saturdays
workday_coefficients <- cleaned_data$coefs[, which(reg_tibble$weekday %in% c('Dienstag', 'Mittwoch', 'Donnerstag'))]
saturday_coefficients <- cleaned_data$coefs[, which(reg_tibble$weekday == 'Samstag')]
