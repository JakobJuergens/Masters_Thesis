# Load necessary packages
library(tidyverse)
library(stringr)
library(lubridate)

# create cluster object
cl <- parallel::makeForkCluster(parallel::detectCores())

# set parameters
approxQ <- 500
n_func <- 4000
seeds <- sample(1:10e5, size = approxQ)

# read in data objects
cleaned_data <- readRDS('../../Data/cleaned_electricity_demand_fd.RDS')
reg_tibble <- readRDS('../../Data/electricity_demand_reg_tibble_holiday_cleaned.RDS')

# extract coefficient objects for weekdays and saturdays
workday_coefficients <- cleaned_data$coefs[, which(reg_tibble$weekday %in% c('Dienstag', 'Mittwoch', 'Donnerstag'))]
saturday_coefficients <- cleaned_data$coefs[, which(reg_tibble$weekday == 'Samstag')]

# calculate t-values using the package PermFDAtest
# first: values for the means based test
nu_vals <- PermFDATest::nu_test_par(
  cl = cl, seeds = seeds,
  approxQ = approxQ,
  sample1 = workday_coefficients,
  sample2 = saturday_coefficients,
  domain = c(0, 24)
)
message('Values of the Mean-based test statistic calculated.')

# evaluate functions at tight grid
grid <- seq(from = 0, to = 24, by = 0.01)
workdays <- purrr::map(
  .x = 1:ncol(workday_coefficients),
  .f = ~ list(
    args = grid,
    vals = PermFDATest::fourier_eval(x = grid, coefs = workday_coefficients[, .x], domain = c(0, 24))
  )
)
saturdays <- purrr::map(
  .x = 1:ncol(saturday_coefficients),
  .f = ~ list(
    args = grid,
    vals = PermFDATest::fourier_eval(x = grid, coefs = saturday_coefficients[, .x], domain = c(0, 24))
  )
)

# second: values for the Cramer von Mises test
# and the objects necessary to calculate them
w_func <- PermFDATest::w_func_construct_1(sample = workdays)
CvM_rho <- PermFDATest::rho_construct(sample = workday_coefficients, factor = 2)

tau_vals <- PermFDATest::tau_realizations_alt(
  approxQ = approxQ, n_basis = 25,
  sample1 = workdays,
  sample2 = saturdays,
  domain = c(0, 24), w_func = w_func, rho = CvM_rho,
  u_sample_func = PermFDATest::u_norm, n_func = n_func
)
message('Values of the CvM test statistic calculated.')

# save t_stats for further processing
t_stats <- list(nu_vals = nu_vals$nu_hat, nu_real = nu_vals$nu_realized,
                tau_vals = tau_vals$tau_hat, tau_real = tau_vals$tau_realized)
saveRDS(
  object = t_stats,
  file = '../../Data/Application_t_stats.RDS'
)

# stop cluster object
parallel::stopCluster(cl)
