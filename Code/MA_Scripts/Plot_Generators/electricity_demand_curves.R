# load necessary packages
library(tidyverse)
library(fds)
library(fda)
library(stringr)

# load data for weekdays
weekday_demand <- list(
  # mon_demand = fds::mondaydemand,
  tue_demand = fds::tuesdaydemand,
  wed_demand = fds::wednesdaydemand,
  thu_demand = fds::thursdaydemand # ,
  # fri_demand = fds::fridaydemand
)

# load data for weekends
weekend_demand <- list(
  sat_demand = fds::saturdaydemand,
  sun_demand = fds::sundaydemand
)

# create basis for functional representation
# Fourier basis should work well due to approximately cyclical data
fourier_basis <- fda::create.fourier.basis(
  rangeval = c(0, 24), nbasis = 25, period = 24
)

# express demand data as functional objects
f_weekday_demand <- purrr::map(
  .x = weekday_demand,
  .f = function(day_demand) {
    fda::smooth.basis(
      argvals = seq(0.5, 24, by = 0.5),
      y = day_demand$y,
      fdParobj = fourier_basis
    )
  }
)
f_weekend_demand <- purrr::map(
  .x = weekend_demand,
  .f = function(day_demand) {
    fda::smooth.basis(
      argvals = seq(0.5, 24, by = 0.5),
      y = day_demand$y,
      fdParobj = fourier_basis
    )
  }
)

# extract elements for wednesdays and saturdays
f_wed_demand <- f_weekday_demand$wed_demand$fd
f_sat_demand <- f_weekend_demand$sat_demand$fd

# create vectors for curves
grid <- seq(0, 24, by = 0.1)
wed_curve <- cbind(
  observation = paste0("wed_obs_", 1:ncol(f_wed_demand$coefs)),
  as_tibble(t(fda::eval.fd(evalarg = grid, fdobj = f_wed_demand)))
)

sat_curve <- cbind(
  observation = paste0("sat_obs_", 1:ncol(f_sat_demand$coefs)),
  as_tibble(t(fda::eval.fd(evalarg = grid, fdobj = f_sat_demand)))
)

names(wed_curve)[-1] <- (1:length(grid))/(length(grid)/24)
names(sat_curve)[-1] <- (1:length(grid))/(length(grid)/24)

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
demand_plot <- ggplot(data = plot_tibble) +
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

# save plot in appropriate folder
ggsave(
  filename = "../../Graphics/electricity_demand_curves.PDF", plot = demand_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
