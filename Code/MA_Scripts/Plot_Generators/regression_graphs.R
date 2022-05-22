# load necessary packages
library(tidyverse)
library(fda)
library(stringr)
library(lubridate)
library(patchwork)

# load in regression object
reg_obj <- readRDS('../../Data/electricity_demand_fReg.RDS')

# extract estimates
estimates <- reg_obj$betaestlist

tibble_names <- c('Constant', 'Year', 'February', 'March',
                'April', 'May', 'June', 'July', 'August',
                'September', 'October', 'November', 'December',
                'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                'Saturday')

# create tibbles for each estimate
grid <- seq(from = 0, to = 24, by = 0.1)
estimate_tibbles <- purrr::map(
  .x = 1:length(estimates),
  .f = function(i){
    tibble(x = grid,
           y = as.vector(fda::eval.fd(
            evalarg = grid,
            fdobj = estimates[[i]]$fd
          )),
          type = tibble_names[i]
      )
  })

# combine tibbles that belong together
plot_list <- list(
  const_year_tibble = rbind(estimate_tibbles[[1]], estimate_tibbles[[2]]),
  month_tibble = bind_rows(estimate_tibbles[3:13]),
  weekday_tibble = bind_rows(estimate_tibbles[14:19])
)

# create vector of plot_titles
plot_titles <- c(
  'Constant and Year Estimates',
  'Month Estimates (January Baseline)',
  'Weekday Estimates (Sunday Baseline)'
)

# change ordering of months and weekdays
plot_list[[2]]$type <- factor(
  x = plot_list[[2]]$type,
  levels = c('February', 'March',
             'April', 'May', 'June', 'July', 'August',
             'September', 'October', 'November', 'December')
  )
plot_list[[3]]$type <- factor(
  x = plot_list[[3]]$type,
  levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
             'Saturday')
  )

# create plot from each tibble
estimate_plots <- purrr::map(
  .x = 2:length(plot_list),
  .f = function(i){
    ggplot(data = plot_list[[i]]) +
      geom_line(aes(x = x, y = y, col = type))+
      theme_light() +
      scale_x_continuous(breaks=seq(0, 24, 4)) +
      ggtitle(label = plot_titles[i]) +
      labs(y = element_blank()) +
      labs(x = element_blank()) +
      theme(
        legend.position = 'bottom',
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 26),
      ) +
      guides(color = guide_legend(override.aes = list(lwd = 5)))
  })

# treat constant and year plot differently
const_year_plot <- (ggplot(data = estimate_tibbles[[1]]) +
  geom_line(aes(x = x, y = y)) +
  theme_light() +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  ggtitle(label = "Constant") +
  labs(y = element_blank()) +
  labs(x = element_blank()) +
  theme(
    legend.position = c(.08, .8),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 30),
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26),
  ) +
  guides(color = guide_legend(override.aes = list(lwd = 5)))) |
  (ggplot(data = estimate_tibbles[[2]]) +
    geom_line(aes(x = x, y = y)) +
    theme_light() +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    ggtitle(label = "Year") +
    labs(y = element_blank()) +
    labs(x = element_blank()) +
    theme(
      legend.position = c(.08, .8),
      legend.text = element_text(size = 26),
      legend.title = element_text(size = 30),
      plot.title = element_text(size = 30),
      axis.title = element_text(size = 26),
      axis.text = element_text(size = 26),
    ) +
    guides(color = guide_legend(override.aes = list(lwd = 5))))

# export plots
ggsave(
  filename = "../../Graphics/estimate_const_year.PDF", plot = const_year_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
ggsave(
  filename = "../../Graphics/estimate_months.PDF", plot = estimate_plots[[1]],
  width = 20, height = 10, units = "in", dpi = 600
)
ggsave(
  filename = "../../Graphics/estimate_weekdays.PDF", plot = estimate_plots[[2]],
  width = 20, height = 10, units = "in", dpi = 600
)
