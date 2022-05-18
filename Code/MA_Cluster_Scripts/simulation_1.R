### This script implements the first simluation as described in my master's
# thesis, it is called using sim_1.sh

# set relevant paths
input_path <- "Inputs/"
output_path <- "Outputs/"

# read in inputs
inputs <- readRDS(paste0(input_path, "inputs.RDS"))
n_basis <- inputs$n_basis
sample_size <- inputs$sample_size
approxQ <- inputs$approxQ
n_func <- inputs$n_func
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma
comparison_grid <- inputs$comparison_grid

# read in seeds and generate string version
full_seeds <- readRDS(paste0(input_path, "seeds.RDS"))
string_seeds <- sapply(full_seeds, toString)

# get environment variables from SLURM
task_id <- strtoi(Sys.getenv("SLURM_ARRAY_TASK_ID"))
task_count <- strtoi(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
n_cores <- strtoi(Sys.getenv("SLURM_CPUS_PER_TASK"))

# Get seeds to use in this task
task_seeds_int <- full_seeds[seq(from = task_id, to = length(full_seeds), by = task_count)]
task_seeds_str <- string_seeds[seq(from = task_id, to = length(full_seeds), by = task_count)]
n_runs <- length(task_seeds_int)

# Define Simulation Function
main_simu <- function(seed) {
  message(paste0('Current Seed: ', seed))
  # set seed for replication purposes
  set.seed(seed)
  
  # generate samples for procedure
  samples <- PermFDATest::sim_1_generator(
    n_basis = n_basis, n_obs_1 = sample_size, n_obs_2 = sample_size, 
    mean = gen_mean, grid = gen_grid, rho = gen_rho, sigma = gen_sigma
  )
  message('Samples Generated')
  
  # calculate t-values using the package PermFDAtest
  # first: values for the means based test
  nu_vals <- PermFDATest::nu_realizations(
    full = FALSE, approxQ = approxQ, 
    sample1 = samples$sample_1_f,
    sample2 = samples$sample_2_f,
    domain = c(0, 1)
  )
  
  # second: values for the Cramer von Mises test
  # and the objects necessary to calculate them
  w_func <- function(x) {
    median(x = unlist(
      purrr::map(
        .x = samples$sample_1, .f = ~ max(.x$vals)
      )
    )
    )
  }
  CvM_rho <- unlist(
    purrr::map(
      .x = 1:n_basis, 
      .f = ~ sd(samples$sample_1_f[.x,])
    )
  )*2
  
  tau_vals <- PermFDATest::tau_realizations(
    full = FALSE, approxQ = approxQ, 
    sample1 = samples$sample_1_f,
    sample2 = samples$sample_2_f,
    type = "fourier", domain = c(0, 1), w_func = w_func, rho = CvM_rho, 
    u_sample_func = PermFDATest::u_norm, n_func = n_func
  )
  message('Values of the CvM test statistic calculated.')
  
  # save t_stats for further processing
  t_stats <- list(samples = samples, 
                  nu_vals = nu_vals$nu_hat, nu_real = nu_vals$nu_realized,
                  tau_vals = tau_vals$tau_hat, tau_real = tau_vals$tau_realized)
  saveRDS(
    object = t_stats,
    file = paste0(output_path, "simulation_1/", toString(seed), "tstats.RDS")
  )
}

# Generate Text Output for Information Purposes
print(paste0(
  "Job started: Job_ID: ", Sys.getenv("SLURM_JOB_ID"),
  " Running on node: ", Sys.getenv("SLURM_NODENAME")
))

start <- Sys.time()
# perform simulations
for (i in 1:n_runs) {
  print(paste0("Run ", i, " of ", n_runs, "."))
  main_simu(seed = task_seeds_int[i])
}
end <- Sys.time()