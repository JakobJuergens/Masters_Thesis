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

# create tibbles for each estimate
grid <- seq(from = 0, to = 24, by = 0.1)
estimate_tibbles <- purrr::map(
  .x = estimates,
  .f = function(fd_obj){
    tibble(x = grid, y = fda::eval.fd(
      evalarg = grid,
      fdobj = fd_obj$fd)
      )
  })

# create vector of plot names
plot_names <- c('Constant', 'Year', 'February', 'March',
                'April', 'May', 'June', 'July', 'August',
                'September', 'October', 'November', 'December',
                'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                'Saturday', 'Sunday')

# create plot from each tibble
estimate_plots <- purrr::map(
  .x = 1:length(estimate_tibbles),
  .f = function(i){
    ggplot(data = estimate_tibbles[[i]]) +
      geom_line(aes(x = x, y = y))+
      theme_light() +
      scale_x_continuous(breaks=seq(0, 24, 4)) +
      ggtitle(label = plot_names[i]) +
      labs(y = element_blank()) +
      labs(x = element_blank()) +
      theme(
        legend.position = c(.08,.8),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 26),
      ) +
      guides(color = guide_legend(override.aes = list(lwd = 5)))
  })

# constant and year plot
const_year_plot <- estimate_plots[[1]] | estimate_plots[[2]]
month_plot <- (estimate_plots[[3]] | estimate_plots[[4]] | estimate_plots[[5]]) /
              (estimate_plots[[6]] | estimate_plots[[7]] | estimate_plots[[8]]) /
              (estimate_plots[[9]] | estimate_plots[[10]] | estimate_plots[[11]]) /
              (estimate_plots[[12]] | estimate_plots[[13]])
wd_plot <- (estimate_plots[[14]] | estimate_plots[[15]]) /
           (estimate_plots[[16]] | estimate_plots[[17]]) /
           (estimate_plots[[18]] | estimate_plots[[19]])

# export plots
ggsave(
  filename = "../../Graphics/estimate_const_year.PDF", plot = const_year_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
ggsave(
  filename = "../../Graphics/estimate_months.PDF", plot = month_plot,
  width = 20, height = 25, units = "in", dpi = 600
)
ggsave(
  filename = "../../Graphics/estimate_weekdays.PDF", plot = wd_plot,
  width = 20, height = 25, units = "in", dpi = 600
)
