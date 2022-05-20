# Load necessary packages
library(fds)
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# perform functional regression
regression_fd <- with(reg_tibble, large_functional_data)
trend_cycl_reg <- fda::fRegress(regression_fd ~ year + month + weekday, data = reg_tibble)
trend_cycl_reg_no_weekday <- fda::fRegress(regression_fd ~ year + month, data = reg_tibble)

# extract object without trend and month cyclical component
cleaned_coefs <- trend_cycl_reg_no_weekday$yfdobj$coefs - trend_cycl_reg_no_weekday$yhatfdobj$coefs

# generate fd object from this
cleaned_fd <- fda::fd(coef = cleaned_coefs, basisobj = fourier_basis)
saveRDS(cleaned_fd, '../../Data/cleaned_electricity_demand_fd.RDS')

# extract coefficient matrices for days of interest
weekday_coef_list <- list(
  monday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Montag")],
  tuesday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Dienstag")],
  wednesday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Mittwoch")],
  thursday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Donnerstag")],
  friday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Freitag")],
  saturday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Samstag")],
  sunday_coefs = cleaned_coefs[, which(reg_tibble$weekday == "Sonntag")]
)

saveRDS(weekday_coef_list, '../../Data/cleaned_electricity_demand_list.RDS')
