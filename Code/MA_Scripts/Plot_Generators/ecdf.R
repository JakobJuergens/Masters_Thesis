library(tidyverse)

n_obs <- c(5, 25, 100)

samples <- purrr::map(
  .x = n_obs,
  .f = ~ rnorm(n = .x)
)

min <- min(unlist(purrr::map(.x = samples, .f = ~min(.x))))
max <- max(unlist(purrr::map(.x = samples, .f = ~max(.x))))
border <- ceiling(max(c(-min, max)))

ecdfs <- purrr::map(
  .x = samples,
  .f = ~ ecdf(.x)
)

plot_grid <- seq(from = -border, to = border, length.out = 1001)

plot_tibble <- rbind(
  bind_rows(
    purrr::map(
      .x = 1:length(ecdfs),
      .f = ~ tibble(
        id = paste0("n = ", n_obs[.x]),
        x = plot_grid,
        y = ecdfs[[.x]](plot_grid)
      )
    )
  ),
  tibble(
    id = "Standard Normal - CDF",
    x = plot_grid,
    y = pnorm(q = plot_grid)
  )
)

plot_tibble$id <- factor(plot_tibble$id, levels = c(paste0("n = ", n_obs), "Standard Normal - CDF"))

ecdf_plot <- ggplot(data = plot_tibble) +
  geom_line(aes(x = x, y = y, col = id), lwd = 1) +
  theme_light() +
  theme(
    legend.position = 'bottom',
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 30),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26),
  ) +
  guides(color = guide_legend(override.aes = list(lwd = 5)))

ggsave(
  filename = "../../Graphics/ecdf.PDF", plot = ecdf_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
