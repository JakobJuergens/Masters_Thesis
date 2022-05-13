### This script implements the first simluation as described in my master's
# thesis, it is called using sim_1.sh

# set relevant paths
input_path <- "Inputs/"
output_path <- "Outputs/"

# read in inputs
inputs <- readRDS(paste0(input_path, "inputs.RDS"))
sample_size <- inputs$sample_size
approxQ <- inputs$approxQ
n_func <- inputs$n_func
gen_grid <- inputs$gen_grid
gen_mean <- inputs$gen_mean
gen_rho <- inputs$gen_rho
gen_sigma <- inputs$gen_sigma
sigma_shift <- inputs$sigma_shift

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
main_simu <- function(cl = cl, seed = task_seeds_int[i]) {
  # set seed for replication purposes
  set.seed(seed)

  # generate samples for procedure
  samples <- PermFDATest::sim_4_generator(
    n_obs_1 = sample_size, n_obs_2 = sample_size, mean = gen_mean, grid = gen_grid, rho = gen_rho, sigma = gen_sigma
  )

  # calculate t-values using the package PermFDAtest
  # first: values for the means based test
  nu_vals <- PermFDATest::means_tstats(
    full = FALSE, approxQ = approxQ, sample1 = samples$sample_1, sample2 = samples$sample_2,
    interpolation_mode = "linear", domain = c(0, 1), n_basis = NULL, grid = gen_grid
  )

  # second: values for the Cramer von Mises test
  # and the objects necessary to calculate them
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 15, period = 1)
  w_func <- function(x) {
    return(1)
  }
  CvM_rho <- seq(from = 5, to = 1, length.out = 15)
  tau_vals <- PermFDATest::cramer_von_mises_tstats(
    full = FALSE, approxQ = approxQ, sample1 = samples$sample_1_f, sample2 = samples$sample_2_f,
    type = "fourier", domain = c(0, 1), basis = fourier_basis, grid = NULL, eigen_func_obj = NULL,
    w_func = w_func, rho = CvM_rho, u_sample_func = PermFDATest::u_norm, n_func = n_func
  )

  # save t_stats for further processing
  t_stats <- list(nu_vals = nu_vals, tau_vals = tau_vals)
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

# create Cluster with parallel package
cl <- parallel::makeForkCluster(n_cores)
print(paste0("R-Cluster created: ", n_cores, " cores."))

# call Package written for Master's Thesis in every forked instance
clusterCall(cl = cl, fun = function() suppressMessages(library(PermFDATest)))
print("Library SurvivR called in individual sessions.")

# perform simulations
for (i in 1:n_runs) {
  print("")
  main_simu(cl = cl, seed = task_seeds_int[i])
}

# Stop cluster and print finishing message
print("")
parallel::stopCluster(cl)
print("R-Cluster stopped.")
rm(cl)
