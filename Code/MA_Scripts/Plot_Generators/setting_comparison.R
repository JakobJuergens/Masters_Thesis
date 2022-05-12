# This script generates a plot that compares data sets generated for the different
# simulation settings

# read in input from simulation project
inputs <- readRDS("../MA_Cluster_Scripts/Inputs/inputs.RDS")
sample_size <- inputs$sample_size
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma

# set parameters for plotting (currently manually here)
mean_shift <- seq(from = 0, to = 1, length.out = length(gen_grid))
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
samples_set_3 <- "NA"
samples_set_4 <- "NA"

# recombine into typical fda shape for easier evaluation
transf_samples_set1 <- purrr::map(.x = samples_set1[3:4], .f = PermFDATest::func_sample_transform2)
transf_samples_set2 <- purrr::map(.x = samples_set2[3:4], .f = PermFDATest::func_sample_transform2)
transf_samples_set3 <- "NA"
transf_samples_set4 <- "NA"

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
eval_samples_set3 <- "NA"
eval_samples_set4 <- "NA"

# combine into tibbles for plotting
plot_tibble_set1 <- dplyr::bind_rows(eval_samples_set1)
plot_tibble_set2 <- dplyr::bind_rows(eval_samples_set2)
plot_tibble_set3 <- "NA"
plot_tibble_set4 <- "NA"

# generate plotting list
plot_list <- list(setting_1 = plot_tibble_set1, setting_2 = plot_tibble_set2)

# generate plots
plots <- purrr::map(
  .x = plot_list,
  .f = ~ (ggplot(data = .x) +
    geom_line(aes(x = x, y = y, col = id, group = Observation)) +
    theme_light() +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
    theme(
      legend.position = 'none',
      plot.title = element_text(size = 30),
      axis.title = element_text(size = 26),
      axis.text = element_text(size = 26),
    ) +
    guides(color = guide_legend(override.aes = list(lwd = 5))))
)

# save plots
purrr::map(.x = 1:length(plots),
           .f = ~ ggsave(
             filename = paste0("../../Graphics/Setting_", .x,".PDF"), plot = plots[[.x]],
             width = 20, height = 6, units = "in", dpi = 600
           ))
