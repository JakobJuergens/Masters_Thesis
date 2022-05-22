# Load necessary packages
library(fds)
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# load data into list
# These data sets consist of half-hourly electricity demands from Sunday to
# Saturday in Adelaide between 6/7/1997 and 31/3/2007.
demands <- list(
  sun_demand = fds::sundaydemand,
  mon_demand = fds::mondaydemand,
  tue_demand = fds::tuesdaydemand,
  wed_demand = fds::wednesdaydemand,
  thu_demand = fds::thursdaydemand,
  fri_demand = fds::fridaydemand,
  sat_demand = fds::saturdaydemand
)

# translate to list of functional observations
# create basis for functional representation
# Fourier basis should work well due to approximately cyclical data
fourier_basis <- fda::create.fourier.basis(
  rangeval = c(0, 24), nbasis = 25, period = 24
)

# express demand data as functional objects
f_demands <- purrr::map(
  .x = demands,
  .f = function(day_demand) {
    fda::smooth.basis(
      argvals = seq(0.5, 24, by = 0.5),
      y = day_demand$y,
      fdParobj = fourier_basis
    )
  }
)

# put into useful format
f_demand_list <- list()
coefficients <- list()
my_basis <- f_demands[[1]]$fd$basis
for(i in 1:7){
  # get coefficients
  coefficients[[i]] <- f_demands[[i]]$fd$coefs
}
coefficient_matrix <- matrix(data = NA, nrow = 25, ncol = 7*508)
for(i in 1:(7*508)){
  # combine into one coefficient matrix
  coefficient_matrix[,i] <- coefficients[[((i-1) %% 7) + 1]][ , (i-1)%/%7 + 1]
}
# create functional object from combined matrix
large_functional_data <- fda::fd(coef = coefficient_matrix, basisobj = fourier_basis)

saveRDS(large_functional_data, '../../Data/fda_all_days.RDS')

# create regressor tibble
reg_tibble <- tibble(
  constant = rep(1, times = ncol(large_functional_data$coefs)),
  DAY = 1:ncol(large_functional_data$coefs),
  DATE = as_date(DAY, origin = as_date('1997-07-05')),
  month = as_factor(month(DATE)),
  weekday = as_factor(weekdays(DATE)),
  year = year(DATE) - 1997
) %>%
  mutate(rownumber = 1:n()) %>%
  group_by(year, month, weekday) %>%
  mutate(n_wd = 1:n())

saveRDS(reg_tibble, '../../Data/electricity_demand_reg_tibble.RDS')
