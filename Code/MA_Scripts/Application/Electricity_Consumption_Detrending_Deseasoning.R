# Load necessary packages
library(fds)
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# read in data
large_functional_data <- readRDS('../../Data/fda_all_days_holiday_cleaned.RDS')
reg_tibble <- readRDS('../../Data/electricity_demand_reg_tibble_holiday_cleaned.RDS')

# create fourier basis
fourier_basis <- fda::create.fourier.basis(
  rangeval = c(0, 24), nbasis = 25, period = 24
)

# perform functional regression
regression_fd <- with(reg_tibble, large_functional_data)
trend_cycl_reg <- fda::fRegress(regression_fd ~ year + month + weekday, data = reg_tibble)

saveRDS(trend_cycl_reg, '../../Data/electricity_demand_fReg.RDS')

# extract object without trend and month cyclical component
# this has to be altered
residual_coefs <- trend_cycl_reg$yfdobj$coefs - trend_cycl_reg$yhatfdobj$coefs
weekday_coefs <- matrix(
  data = unlist(
    purrr::map(
      .x = 1:nrow(reg_tibble),
      .f = function(i) {
        if (reg_tibble$weekday[i] == "Montag") {
          return(trend_cycl_reg$betaestlist$weekday.Montag$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Dienstag") {
          return(trend_cycl_reg$betaestlist$weekday.Dienstag$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Mittwoch") {
          return(trend_cycl_reg$betaestlist$weekday.Mittwoch$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Donnerstag") {
          return(trend_cycl_reg$betaestlist$weekday.Donnerstag$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Freitag") {
          return(trend_cycl_reg$betaestlist$weekday.Freitag$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Samstag") {
          return(trend_cycl_reg$betaestlist$weekday.Samstag$fd$coefs)
        } else if (reg_tibble$weekday[i] == "Sonntag") {
          return(rep(x = 0, times = nrow(residual_coefs)))
        }
      }
    )
  ), nrow = nrow(residual_coefs), ncol = ncol(residual_coefs), byrow = FALSE
)

cleaned_coefs <- matrix(
  data = rep(trend_cycl_reg$betaestlist$const$fd$coefs, times = ncol(residual_coefs)),
  nrow = nrow(residual_coefs), ncol = ncol(residual_coefs), byrow = FALSE) + residual_coefs + weekday_coefs

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
