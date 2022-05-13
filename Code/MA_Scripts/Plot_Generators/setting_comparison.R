# This script generates a plot that compares data sets generated for the different
# simulation settings

library(tidyverse)
library(patchwork)

# read in input from simulation project
inputs <- readRDS("../MA_Cluster_Scripts/Inputs/inputs.RDS")
sample_size <- inputs$sample_size
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma

# set parameters for plotting (currently manually here)
plot_grid <- seq(from = 0, to = 1, length.out = 1001)

# generate samples
samples_set1 <- PermFDATest::sim_1_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size,
  mean = gen_mean, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
)
samples_set2 <- PermFDATest::sim_2_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size,
  mean = gen_mean, mean_shift = mean_shift, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
)
samples_set3 <- PermFDATest::sim_3_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size,
  mean = gen_mean, rho = gen_rho, rho_shift = rho_shift, sigma = gen_sigma, grid = gen_grid
)
samples_set4 <- PermFDATest::sim_4_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size,
  mean = gen_mean, rho = gen_rho, sigma = gen_sigma, sigma_shift = sigma_shift, grid = gen_grid
)

# recombine into typical fda shape for easier evaluation
transf_samples_set1 <- purrr::map(.x = samples_set1[3:4], .f = PermFDATest::func_sample_transform2)
transf_samples_set2 <- purrr::map(.x = samples_set2[3:4], .f = PermFDATest::func_sample_transform2)
transf_samples_set3 <- purrr::map(.x = samples_set3[3:4], .f = PermFDATest::func_sample_transform2)
transf_samples_set4 <- purrr::map(.x = samples_set4[3:4], .f = PermFDATest::func_sample_transform2)

# evaluate the functional objects
eval_samples_set1 <- purrr::map(
  .x = 1:length(transf_samples_set1),
  .f = ~ pivot_longer(
    data = cbind(
      id = paste0("Sample ", .x),
      x = plot_grid,
      tibble::as_tibble(
        fda::eval.fd(evalarg = plot_grid, fdobj = transf_samples_set1[[.x]])
      )
    ),
    cols = !c(id, x), values_to = "y", names_to = "Observation"
  ) %>% mutate(Observation = paste0(Observation, id))
)
eval_samples_set2 <- purrr::map(
  .x = 1:length(transf_samples_set2),
  .f = ~ pivot_longer(
    data = cbind(
      id = paste0("Sample ", .x),
      x = plot_grid,
      tibble::as_tibble(
        fda::eval.fd(evalarg = plot_grid, fdobj = transf_samples_set2[[.x]])
      )
    ),
    cols = !c(id, x), values_to = "y", names_to = "Observation"
  ) %>% mutate(Observation = paste0(Observation, id))
)
eval_samples_set3 <- purrr::map(
  .x = 1:length(transf_samples_set3),
  .f = ~ pivot_longer(
    data = cbind(
      id = paste0("Sample ", .x),
      x = plot_grid,
      tibble::as_tibble(
        fda::eval.fd(evalarg = plot_grid, fdobj = transf_samples_set3[[.x]])
      )
    ),
    cols = !c(id, x), values_to = "y", names_to = "Observation"
  ) %>% mutate(Observation = paste0(Observation, id))
)
eval_samples_set4 <- purrr::map(
  .x = 1:length(transf_samples_set4),
  .f = ~ pivot_longer(
    data = cbind(
      id = paste0("Sample ", .x),
      x = plot_grid,
      tibble::as_tibble(
        fda::eval.fd(evalarg = plot_grid, fdobj = transf_samples_set4[[.x]])
      )
    ),
    cols = !c(id, x), values_to = "y", names_to = "Observation"
  ) %>% mutate(Observation = paste0(Observation, id))
)

# combine into tibbles for plotting
plot_tibble_set1 <- dplyr::bind_rows(eval_samples_set1)
plot_tibble_set2 <- dplyr::bind_rows(eval_samples_set2)
plot_tibble_set3 <- dplyr::bind_rows(eval_samples_set3)
plot_tibble_set4 <- dplyr::bind_rows(eval_samples_set4)

# generate plotting list
plot_list <- list(
  setting_1 = plot_tibble_set1, setting_2 = plot_tibble_set2,
  setting_3 = plot_tibble_set3, setting_4 = plot_tibble_set4
)

# generate plots
plots <- purrr::map(
  .x = 1:length(plot_list),
  .f = ~ (ggplot(data = plot_list[[.x]]) +
    geom_line(aes(x = x, y = y, col = id, group = Observation)) +
    theme_light() +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
    labs(title = paste0("Setting ", .x)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 30),
      axis.title = element_blank(),
      axis.text = element_text(size = 26),
    ) +
    guides(color = guide_legend(override.aes = list(lwd = 5))))
)

# determine limits for y-axis
y_min <- min(unlist(purrr::map(.x = plot_list, .f = ~ min(.x$y))))
y_max <- max(unlist(purrr::map(.x = plot_list, .f = ~ max(.x$y))))

# combine plots using patchwork
comb_plot <- ((plots[[1]] | plots[[2]]) /
              (plots[[3]] | plots[[4]]))  &
  ylim(y_min, y_max)

# save plots
ggsave(
  filename = paste0("../../Graphics/Settings_comparison.PDF"), plot = comb_plot,
  width = 20, height = 12, units = "in", dpi = 600
)
