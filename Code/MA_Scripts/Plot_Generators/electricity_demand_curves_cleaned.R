# load necessary packages
library(tidyverse)
library(fds)
library(fda)
library(stringr)

# load cleaned_fd object
cleaned_fd <- readRDS('../../Data/cleaned_electricity_demand_fd.RDS')
reg_tibble <- readRDS('../../Data/electricity_demand_reg_tibble_holiday_cleaned.RDS')

# extract relevant coefficients for wednesdays and saturdays
wednesday_index <- which(reg_tibble$weekday == 'Mittwoch')
wednesday_coefficients <- cleaned_fd$coefs[, wednesday_index]

saturday_index <- which(reg_tibble$weekday == 'Samstag')
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

# save plot in appropriate folder
ggsave(
  filename = "../../Graphics/electricity_demand_curves_cleaned.PDF", plot = demand_plot_cleaned,
  width = 20, height = 6, units = "in", dpi = 600
)
