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

# create regressor tibble
reg_tibble <- tibble(
  constant = rep(1, times = ncol(large_functional_data$coefs)),
  DAY = 1:ncol(large_functional_data$coefs),
  DATE = as_date(DAY, origin = as_date('1997-07-05')),
  month = as_factor(month(DATE)),
  weekday = as_factor(weekdays(DATE)),
  year = year(DATE) - 1997
) %>%
  select(!c(DAY, DATE))

# perform functional regression
regression_fd <- with(reg_tibble, large_functional_data)
# trend_cycl_reg <- fda::fRegress(regression_fd ~ year + month + weekday, data = reg_tibble)
trend_cycl_reg_no_weekday <- fda::fRegress(regression_fd ~ year + month, data = reg_tibble)

# extract object without trend and month cyclical component
cleaned_coefs <- trend_cycl_reg_no_weekday$yfdobj$coefs - trend_cycl_reg_no_weekday$yhatfdobj$coefs

# generate fd object from this
cleaned_fd <- fda::fd(coef = cleaned_coefs, basisobj = fourier_basis)

# extract relevant coefficients for wednesdays and saturdays
# remember observations start on sunday(!)
wednesday_index <- 3 + seq(from = 1, to = 7*52 + 1, by = 7)
wednesday_coefficients <- cleaned_fd$coefs[, wednesday_index]

saturday_index <- 6 + seq(from = 1, to = 7*52 + 1, by = 7)
saturday_coefficients <- cleaned_fd$coefs[, saturday_index]

# generate their own fd objects
wednesday_fd <- fda::fd(coef = wednesday_coefficients, basisobj = cleaned_fd$basis)
saturday_fd <- fda::fd(coef = saturday_coefficients, basisobj = cleaned_fd$basis)

# evaluate fd objects at chosen grid
grid <- seq(0, 24, by = 0.1)
wed_curve <- cbind(
  observation = paste0("wed_obs_", 1:ncol(wednesday_fd$coefs)),
  as_tibble(t(fda::eval.fd(evalarg = grid, fdobj = wednesday_fd)))
)

sat_curve <- cbind(
  observation = paste0("sat_obs_", 1:ncol(saturday_fd$coefs)),
  as_tibble(t(fda::eval.fd(evalarg = grid, fdobj = saturday_fd)))
)

names(wed_curve)[-1] <- grid
names(sat_curve)[-1] <- grid

# bring into long format for plotting
wed_plot <- wed_curve %>%
  pivot_longer(cols = !observation, names_to = 'Time', values_to = 'Demand')
sat_plot <- sat_curve %>%
  pivot_longer(cols = !observation, names_to = 'Time', values_to = 'Demand')

# add Day column
wed_plot$Day <- 'wednesday'
sat_plot$Day <- 'saturday'

# create combined tibble and use only the first 52 observations for each Day
plot_tibble <- rbind(
  wed_plot %>%
    filter(observation %in% paste0('wed_obs_', 1:52)),
  sat_plot %>%
    filter(observation %in% paste0('sat_obs_', 1:52))
) %>%
  mutate(Time = as.numeric(Time))

# create ggplot
demand_plot_cleaned <- ggplot(data = plot_tibble) +
  geom_line(aes(x = Time, y = Demand, col = Day, group = observation),
            alpha = 0.5) +
  theme_light() +
  scale_x_continuous(breaks=seq(0, 24, 4)) +
  labs(y = 'Demand in Megawatt') +
  theme(
    legend.position = c(.08,.8),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 30),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26),
  ) +
  guides(color = guide_legend(override.aes = list(lwd = 5)))
