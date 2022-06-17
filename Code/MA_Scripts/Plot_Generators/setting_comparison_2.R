# This script generates a plot that compares data sets generated for the different
# simulation settings

library(tidyverse)
library(patchwork)

# read in input from simulation project
inputs <- readRDS("../MA_Cluster_Scripts/Inputs/inputs.RDS")
sample_size <- inputs$sample_size
n_basis <- inputs$n_basis
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma

mean_shift <- inputs$mean_shift
rho_shift <- inputs$rho_shift
sigma_shift <- inputs$sigma_shift

# set parameters for plotting (currently manually here)
plot_grid <- seq(from = 0, to = 1, length.out = 1001)

# generate samples
samples_set1 <- PermFDATest::sim_1_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis,
  mean = gen_mean, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
)
samples_set2 <- PermFDATest::sim_2_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis,
  mean = gen_mean, mean_shift = mean_shift, rho = gen_rho, sigma = gen_sigma, grid = gen_grid
)
samples_set3 <- PermFDATest::sim_3_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis,
  mean = gen_mean, rho = gen_rho, rho_shift = rho_shift, sigma = gen_sigma, grid = gen_grid
)
samples_set4 <- PermFDATest::sim_4_generator(
  n_obs_1 = sample_size, n_obs_2 = sample_size, n_basis = n_basis,
  mean = gen_mean, rho = gen_rho, sigma = gen_sigma, sigma_shift = sigma_shift, grid = gen_grid
)

# recombine into typical fda shape for easier evaluation
transf_samples_set1 <- samples_set1[1:2]
transf_samples_set2 <- samples_set2[1:2]
transf_samples_set3 <- samples_set3[1:2]
transf_samples_set4 <- samples_set4[1:2]

# evaluate the functional objects
eval_samples_set1 <- purrr::map(
  .x = 1:length(transf_samples_set1),
  .f = function(smpl) {
    bind_rows(
      purrr::map(
        .x = 1:length(transf_samples_set1[[smpl]]),
        .f = function(obs) {
          tibble(
            x = transf_samples_set1[[smpl]][[obs]]$args,
            y = transf_samples_set1[[smpl]][[obs]]$vals,
            obs = paste0('Sample_', smpl, '_Obs_', obs),
            sample = paste0('Sample ', smpl)
          )
        }
      )
    )
  }
)

eval_samples_set2 <- purrr::map(
  .x = 1:length(transf_samples_set2),
  .f = function(smpl) {
    bind_rows(
      purrr::map(
        .x = 1:length(transf_samples_set2[[smpl]]),
        .f = function(obs) {
          tibble(
            x = transf_samples_set2[[smpl]][[obs]]$args,
            y = transf_samples_set2[[smpl]][[obs]]$vals,
            obs = paste0('Sample_', smpl, '_Obs_', obs),
            sample = paste0('Sample ', smpl)
          )
        }
      )
    )
  }
)

eval_samples_set3 <- purrr::map(
  .x = 1:length(transf_samples_set3),
  .f = function(smpl) {
    bind_rows(
      purrr::map(
        .x = 1:length(transf_samples_set3[[smpl]]),
        .f = function(obs) {
          tibble(
            x = transf_samples_set3[[smpl]][[obs]]$args,
            y = transf_samples_set3[[smpl]][[obs]]$vals,
            obs = paste0('Sample_', smpl, '_Obs_', obs),
            sample = paste0('Sample ', smpl)
          )
        }
      )
    )
  }
)

eval_samples_set4 <- purrr::map(
  .x = 1:length(transf_samples_set4),
  .f = function(smpl) {
    bind_rows(
      purrr::map(
        .x = 1:length(transf_samples_set4[[smpl]]),
        .f = function(obs) {
          tibble(
            x = transf_samples_set4[[smpl]][[obs]]$args,
            y = transf_samples_set4[[smpl]][[obs]]$vals,
            obs = paste0('Sample_', smpl, '_Obs_', obs),
            sample = paste0('Sample ', smpl)
          )
        }
      )
    )
  }
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
            geom_line(aes(x = x, y = y, col = sample, group = obs)) +
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
  width = 20, height = 10, units = "in", dpi = 600
)
