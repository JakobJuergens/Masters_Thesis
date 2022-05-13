# Set seed for creating of inputs
set.seed(1234)
# Set path for input objects and potential outputs
input_path <- "Inputs/"
output_path <- "Outputs/"
# number of seeds / simulation runs
n_seeds <- 100
# generate seeds and save them as RDS object
my_seeds <- sample(x = 1:10e6, size = n_seeds, replace = FALSE)
saveRDS(object = my_seeds, file = paste0(input_path, "seeds.RDS"))

##### Set parameters for Simulation #####

# set sample size for generated samples
sample_size <- 20
# number of permutations used for the approximation of the test statistcs
approxQ <- 10000
# number of functions used for the Monte-Carlo Integration used in the approximation
# of the test statistic tau for the CvM test
n_func <- 1000
# generate a grid for the data generation
gen_grid <- seq(from = 0, to = 1, length.out = 101)
# generate mean function for the data generation
gen_mean <- rep(x = 0, times = length(gen_grid))
# generate rhos for the data generation
gen_rho <- rep(x = 0.5, times = length(gen_grid))
# generate sigmas for the data generation
gen_sigma <- rep(x = 1, length.out = length(gen_grid))
# generate shift vectors for simulations
mean_shift <- seq(from = -1.1, to = 1.1, length.out = length(gen_grid))^2 - 1.1^2
rho_shift <- rep(x = -0.5, times = length(gen_grid))
sigma_shift <- seq(from = 0, to = 0.5, length.out = length(gen_grid))

# save inputs to list and save in input folder
inputs <- list(
  sample_size = sample_size, approxQ = approxQ, n_func = n_func,
  gen_grid = gen_grid, gen_mean = gen_mean, gen_rho = gen_rho, gen_sigma = gen_sigma,
  mean_shift = mean_shift, rho_shift = rho_shift, sigma_shift = sigma_shift
)
saveRDS(object = inputs, file = paste0(input_path, "inputs.RDS"))
