### This script implements the first simluation as described in my master's
# thesis, it is called using sim_1.sh

# set relevant paths
input_path <- "Inputs/"
output_path <- "Outputs/"

# read in inputs
inputs <- readRDS(paste0(input_path, 'inputs.RDS'))
sample_size <- inputs$sample_size
approxQ <- inputs$approxQ
n_func <- inputs$n_func

# read in seeds and generate string version
full_seeds <- readRDS(paste0(input_path, "seeds.RDS"))
string_seeds <- sapply(full_seeds, toString)

# get environment variables from SLURM
task_id <- strtoi(Sys.getenv("SLURM_ARRAY_TASK_ID"))
task_count <- strtoi(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
n_cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK'))

# Get seeds to use in this task
task_seeds_int <- full_seeds[seq(from = task_id, to = length(full_seeds), by = task_count)]
task_seeds_str <- string_seeds[seq(from = task_id, to = length(full_seeds), by = task_count)]
n_runs <- length(task_seeds_int)

# Define Simulation Function
main_simu <- function(){
  
}

# Generate Text Output for Information Purposes
print(paste0('Job started: Job_ID: ', Sys.getenv('SLURM_JOB_ID'),
             ' Running on node: ', Sys.getenv('SLURM_NODENAME')))

# create Cluster with parallel package
cl <- parallel::makeForkCluster(n_cores)
print(paste0('R-Cluster created: ', n_cores, ' cores.'))

# call Package written for Master's Thesis in every forked instance
clusterCall(cl = cl, fun = function() suppressMessages(library(PermFDATest)))
print('Library SurvivR called in individual sessions.')

# perform simulations
for (i in 1:n_runs) {
  print('')
  main_simu(cl = cl, seed = task_seeds_int[i])
}

# Stop cluster and print finishing message
print('')
parallel::stopCluster(cl)
print('R-Cluster stopped.')
rm(cl)
