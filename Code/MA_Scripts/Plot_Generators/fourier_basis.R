# This script generates a plot illustrating the first k basis
# functions from the Fourier basis of L^2

library(tidyverse)
library(fda)

grid <- seq(0, 1, length.out = 200)
fourier_basis <- create.fourier.basis(rangeval = c(0,1), nbasis = 7)
basis_values <- eval.basis(evalarg = grid,
                           basisobj = fourier_basis)

basis_tibble <- cbind(tibble('x' = grid), as_tibble(basis_values)) %>%
  pivot_longer(cols = !'x', names_to = 'Function', values_to = 'y')

fourier_plot <- ggplot(data = basis_tibble) +
  geom_line(aes(x = x, y = y, col = Function)) +
  theme_light() +
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26),
  )

# save plot in appropriate folder
ggsave(
  filename = "../../Graphics/fourier_basis.PDF", plot = fourier_plot,
  width = 20, height = 6, units = "in", dpi = 600
)
