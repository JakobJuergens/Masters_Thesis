# Load necessary packages
library(fds)
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# read in regression_tibble
reg_tibble <- readRDS("../../Data/electricity_demand_reg_tibble.RDS")
functional_data <- readRDS("../../Data/fda_all_days.RDS")

# find holidays that are never replaced by the following working day
non_replacement_holidays <- reg_tibble %>%
  filter(
    (month == 3 & weekday == "Montag" & n_wd == 2) | # March Public Holiday
      (month == 6 & weekday == "Montag" & n_wd == 2) | # Queen's birthday
      (month == 10 & weekday == "Montag" & n_wd == 1) | # Labour Day
      # filter manually for more complex holidays
      # Good Friday & Easter 1997 - not part of the data set
      # Good Friday & Easter 1998
      (year == 1 & month == 4 & day(DATE) %in% 10:13) |
      # Good Friday & Easter 1999
      (year == 2 & month == 4 & day(DATE) %in% 2:5) |
      # Good Friday & Easter 2000
      (year == 3 & month == 4 & day(DATE) %in% 21:24) |
      # Good Friday & Easter 2001
      (year == 4 & month == 4 & day(DATE) %in% 13:16) |
      # Good Friday & Easter 2002
      (year == 5 & month == 3 & day(DATE) %in% 29:31) |
      (year == 5 & month == 4 & day(DATE) == 1) |
      # Good Friday & Easter 2003
      (year == 6 & month == 4 & day(DATE) %in% 18:21) |
      # Good Friday & Easter 2004
      (year == 7 & month == 4 & day(DATE) %in% 9:12) |
      # Good Friday & Easter 2005
      (year == 8 & month == 4 & day(DATE) %in% 25:28) |
      # Good Friday & Easter 2006
      (year == 9 & month == 4 & day(DATE) %in% 14:17)
      # Good Friday & Easter 2007 - not part of the data set
  )

# find holidays that are potentially replaced by the following working day
pot_replacement_holidays <- reg_tibble %>%
  filter(
    (month == 1 & day(DATE) == 1) | # New Year's
      (month == 1 & day(DATE) == 26) | # Australia Day
      (month == 4 & day(DATE) == 25) | # Anzac Day
      (month == 12 & day(DATE) == 25) | # Christmas Day
      (month == 12 & day(DATE) == 26) | # Boxing Day
      (month == 12 & day(DATE) == 31) # New Year's Eve
  )

# find_substituted holidays
substituted_holidays <- pot_replacement_holidays %>%
  filter(
    (month == 1 & day(DATE) == 1 & weekday %in% c("Samstag", "Sonntag")) | # New Year's
      (month == 1 & day(DATE) == 26 & weekday %in% c("Samstag", "Sonntag")) | # Australia Day
      (month == 4 & day(DATE) == 25 & weekday %in% c("Samstag", "Sonntag")) | # Anzac Day
      (month == 12 & day(DATE) == 25 & weekday %in% c("Samstag", "Sonntag")) | # Christmas Day
      (month == 12 & day(DATE) == 26 & weekday %in% c("Samstag", "Sonntag")) | # Boxing Day
      (month == 12 & day(DATE) == 31 & weekday == "Sonntag") # New Year's Eve
  )

# find replacement days
substitution_holidays <- inner_join(
  x = substituted_holidays %>%
    select(DATE) %>%
    mutate(DATE = as_date(
      ifelse(
        test = weekdays(DATE) == "Samstag",
        yes = DATE %m+% days(2),
        no = DATE %m+% days(1)
      )
    )),
  y = reg_tibble, by = "DATE"
)

# get vector of indices for holidays
holiday_indices <- c(
  non_replacement_holidays$rownumber,
  pot_replacement_holidays$rownumber,
  substitution_holidays$rownumber
)

# add days before and after holidays
drop_indices <- intersect(
  x = sort(
    unique(x = c(holiday_indices, holiday_indices + 1, holiday_indices - 1))
  ), y = reg_tibble$rownumber
)

# create tibble where holidays and days immediately before or after them are removed
reg_tibble_holiday_cleaned <- reg_tibble[-drop_indices,]
functional_data_holiday_cleaned <- fda::fd(
  coef = functional_data$coefs[, -drop_indices],
  basisobj = functional_data$basis)

# save objects for further processing
saveRDS(functional_data_holiday_cleaned, '../../Data/fda_all_days_holiday_cleaned.RDS')
saveRDS(reg_tibble_holiday_cleaned, '../../Data/electricity_demand_reg_tibble_holiday_cleaned.RDS')
