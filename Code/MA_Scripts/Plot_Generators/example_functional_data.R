# This script generates a plot that gives a visiual example of functional
# data to introduce the concept from an intuitive perspective.

library(tidyverse)
library(fda)

data(growth)

male_growth_tibble <- cbind(
  'Age' = rownames(growth$hgtm),
  'Group' = 'male',
  as_tibble(growth$hgtm)) %>%
  pivot_longer(cols = !c(Age, Group), names_to = 'ID', values_to = 'Height')

female_growth_tibble <- cbind(
  'Age' = rownames(growth$hgtf),
  'Group' = 'female',
  as_tibble(growth$hgtf)) %>%
  pivot_longer(cols = !c(Age, Group), names_to = 'ID', values_to = 'Height')

growth_tibble <- rbind(male_growth_tibble, female_growth_tibble) %>%
  mutate(Age = as.numeric(Age),
         Height = as.numeric(Height))

# create plot
growth_plot <- ggplot(data = growth_tibble) +
  geom_line(aes(x = Age, y = Height, col = Group, group = ID),
            alpha = 0.5) +
  theme_light() +
  labs(y = 'Height in cm') +
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
  filename = "../../Graphics/growth_curves.PDF", plot = growth_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
