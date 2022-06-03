# This script generates a plot that compares data sets generated for the different
# simulation settings

library(tidyverse)
library(patchwork)
library(latex2exp)

# read in input from simulation project
inputs <- readRDS("../MA_Cluster_Scripts/Inputs/inputs.RDS")
sample_size <- inputs$sample_size
n_basis <- inputs$n_basis
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma

shifts <- c(-0.9, -0.5, 0, 0.5, 0.9)
rho_shift <- purrr::map(.x = shifts,
                        .f = ~ rep(x = .x, times = length(gen_grid)) - gen_rho)

# set parameters for plotting (currently manually here)
plot_grid <- seq(from = 0, to = 1, length.out = 1001)

# generate samples
sample <- PermFDATest::sim_1_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis,
  mean = gen_mean, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
)

reference_sample <- sample$sample_1_f

samples_sec <- purrr::map(
  .x = rho_shift,
  .f = ~ PermFDATest::sim_3_generator(
    n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis, rho_shift = .x,
    mean = gen_mean, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
  )
)

samples_2 <- purrr::map(.x = samples_sec, .f = ~.x$sample_2_f)

# evaluate the functional objects
eval_sample1 <- pivot_longer(
  data = cbind(
    tibble(
      id = "Reference Sample"
    ),
    x = plot_grid,
    tibble::as_tibble(
      PermFDATest::fourier_eval(
        x = plot_grid,
        coefs = reference_sample,
        domain = c(0, 1)
      )
    )
  ), cols = !c(id, x), values_to = "y", names_to = "Observation"
) %>%
  mutate(Observation = paste0(Observation, id))

eval_samples <- purrr::map(
  .x = 1:length(samples_2),
  .f = ~ pivot_longer(
    data = cbind(
      id = paste0("Sample ", 2),
      x = plot_grid,
      tibble::as_tibble(
        PermFDATest::fourier_eval(
          x = plot_grid,
          coefs = samples_2[[.x]],
          domain = c(0, 1)
        )
      )
    ),
    cols = !c(id, x), values_to = "y", names_to = "Observation"
  ) %>% mutate(Observation = paste0(Observation, id))
)

# combine into tibbles for plotting
plot_tibbles <- purrr::map(
  .x = eval_samples,
  .f = ~ dplyr::bind_rows(.x, eval_sample1)
)

#
artifact_plot <- ggplot(data = plot_tibbles[[1]]) +
                    geom_line(aes(x = x, y = y, col = id, group = Observation)) +
                    theme_light() +
                    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
                    labs(title = paste0('rho = ', shifts[1])) +
                    theme(
                      legend.position = "none",
                      legend.title = element_blank(),
                      legend.text = element_text(size = 26),
                      plot.title = element_text(size = 30),
                      axis.title = element_blank(),
                      axis.text = element_text(size = 26),
                    ) +
                    guides(color = guide_legend(override.aes = list(lwd = 5)))


# save plots
ggsave(
  filename = paste0("../../Graphics/rho0_9_comparison.PDF"), plot = artifact_plot,
  width = 20, height = 6, units = "in", dpi = 600
)

# generate plots
plots <- purrr::map(
  .x = 1:length(plot_tibbles),
  .f = ~ (ggplot(data = plot_tibbles[[.x]]) +
            geom_line(aes(x = x, y = y, col = id, group = Observation), alpha = I(0.5)) +
            theme_light() +
            scale_x_continuous(breaks = seq(0, 1, 0.25)) +
            labs(title = paste0('rho = ', shifts[.x])) +
            theme(
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 26),
              plot.title = element_text(size = 30),
              axis.title = element_blank(),
              axis.text = element_text(size = 26),
            ) +
            guides(color = guide_legend(override.aes = list(lwd = 5))))
)

# determine limits for y-axis
y_min <- min(unlist(purrr::map(.x = plot_tibbles, .f = ~ min(.x$y))))
y_max <- max(unlist(purrr::map(.x = plot_tibbles, .f = ~ max(.x$y))))

#
comb_plot <- (((plots[[1]] | plots[[2]]) /
                plots[[3]] /
                (plots[[4]] | plots[[5]]))  &
  ylim(y_min, y_max)) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# save plots
ggsave(
  filename = paste0("../../Graphics/persistence_comparison_basis.PDF"), plot = comb_plot,
  width = 20, height = 12, units = "in", dpi = 600
)

# plot original observations
original_observations <- dplyr::bind_rows(
  purrr::map(
    .x = 1:length(sample$sample_1),
    .f = ~ tibble(
      id = "Reference",
      Observation = paste0('RefSample_Obs', .x),
      x = sample$sample_1[[.x]]$args,
      y = sample$sample_1[[.x]]$vals
    )
  )
)

original_observations2 <- purrr::map(
  .x = samples_sec,
  .f = function(smpl) {
    dplyr::bind_rows(
      purrr::map(
        .x = 1:length(smpl$sample_2),
        .f = function(i) {
          tibble(
            id = "Sample 2",
            Observation = paste0('Sample2_Obs', i),
            x = smpl$sample_2[[i]]$args,
            y = smpl$sample_2[[i]]$vals
          )
        }
      )
    )
  }
)

plot_tibbles2 <- purrr::map(
  .x = original_observations2,
  .f = ~ rbind(original_observations, .x)
)

plots2 <- purrr::map(
  .x = 1:length(plot_tibbles2),
  .f = ~ (ggplot(data = plot_tibbles2[[.x]]) +
            geom_line(aes(x = x, y = y, col = id, group = Observation), alpha = I(0.5)) +
            theme_light() +
            scale_x_continuous(breaks = seq(0, 1, 0.25)) +
            labs(title = paste0('rho = ', shifts[.x])) +
            theme(
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 26),
              plot.title = element_text(size = 30),
              axis.title = element_blank(),
              axis.text = element_text(size = 26),
            ) +
            guides(color = guide_legend(override.aes = list(lwd = 5))))
)

# determine limits for y-axis
y_min <- min(unlist(purrr::map(.x = plot_tibbles2, .f = ~ min(.x$y))))
y_max <- max(unlist(purrr::map(.x = plot_tibbles2, .f = ~ max(.x$y))))

#
comb_plot2 <- (((plots2[[1]] | plots2[[2]]) /
                 plots2[[3]] /
                 (plots2[[4]] | plots2[[5]]))  &
                ylim(y_min, y_max)) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# save plots
ggsave(
  filename = paste0("../../Graphics/persistence_comparison.PDF"), plot = comb_plot2,
  width = 20, height = 12, units = "in", dpi = 600
)
