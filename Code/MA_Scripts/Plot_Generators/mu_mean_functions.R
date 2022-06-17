# This script generates a plot illustrating the different mean
# functions proposed in my master's thesis for the constructed measure

# Load necessary packages
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)

# generate sample
my_mean <- rep(x = 0, times = 101)
my_rho <- rep(x = 0.5, times = 101)
my_sigma <- rep(x = 1, times = 101)
my_grid <- seq(from = 0, to = 1, length.out = 101)

sample <- PermFDATest::sim_1_generator(
  n_basis = 25, n_obs_1 = 50, n_obs_2 = 50, mean = my_mean, rho = my_rho, sigma = my_sigma, grid = my_grid
  )

sample1 <- sample$sample_1
sample1_f <- sample$sample_1_f

# determine the different mean functions
mu_1 <- median(
  x = unlist(
    purrr::map(
      .x = sample1,
      .f = ~ max(.x$vals)
    )
  )
)

# create functional objects
w_func1 <- Vectorize(PermFDATest::w_func_construct_1(sample = sample1))
w_func2 <- Vectorize(PermFDATest::w_func_construct_3(sample = sample1, q = 0.95))

# create tibbles for plotting
plot_grid <- seq(from = 0, to = 1, by = 0.01)

# evaluate sample at grid points
sample_tibble <- tibble(
    supertype = 'Observations',
    type = paste0("obs_", rep(x = 1:50, each = length(my_grid))),
    x = unlist(
      purrr::map(
        .x = sample1,
        .f = ~.x$args
      )
    ),
    y = unlist(
      purrr::map(
        .x = sample1,
        .f = ~.x$vals
      )
    ),
    alpha = 0.2
  )

mean_curve_tibble <- tibble(
  type = c(
    rep(x = "Type 1", times = length(plot_grid)),
    rep(x = "Type 2", times = length(plot_grid))
  ),
  supertype = type,
  x = rep(x = plot_grid, times = 2),
  y = c(
    w_func1(x = plot_grid),
    w_func2(x = plot_grid)
  ),
  alpha = 1
)

plot_tibble <- rbind(sample_tibble, mean_curve_tibble)

# create plot
mean_func_plot <- ggplot(data = plot_tibble) +
  geom_line(aes(x = x, y = y, col = supertype, alpha = I(alpha), group = type)) +
  theme_light() +
  ggtitle(label = 'Comparisons of Mean Functions') +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(size = 26),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26)
  ) +
  guides(color = guide_legend(override.aes = list(lwd = 5)))

# save plot in appropriate folder
ggsave(
  filename = "../../Graphics/mean_functions.PDF", plot = mean_func_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
