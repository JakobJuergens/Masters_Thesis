# load necessary packages
library(tidyverse)
library(stringr)
library(lubridate)

# define levels of the tests that are performed
tau_levels <- c(0.05, 0.01, 0.001)

# combination function
test_combine <- Vectorize(function(phi1, phi2) {
  if (phi1 == 1 | phi2 == 1) {
    return(1)
  } else if (phi1 == 0 | phi2 == 0) {
    return(phi1 + phi2)
  } else {
    return(1 - (1 - phi1) * (1 - phi))
  }
})

# set path to simulation folders
sim_folders <- "../../Data/Cluster_Simulation_Outputs/Persistence_Test2"
# set Output path
output_path <- "../../Data/Cluster_Simulation_Outputs/Evaluation/"

# get list of simulation folders
individual_simulation_folders <- list.dirs(sim_folders)[-1]

# read in all RDS files in the individual simulations
data_read <- function(path) {
  files <- list.files(paste0(path, "/"))
  data <- purrr::map(
    .x = files,
    .f = ~ readRDS(paste0(path, "/", .x))
  )
  return(data)
}

simulation_results <- purrr::map(
  .x = individual_simulation_folders,
  .f = ~ data_read(.x)
)

# calculate values of the test function
val_test_func <- function(sim_list) {
  # tau test decisions
  res <- as_tibble(
    matrix(
      data = unlist(
        purrr::map(
          .x = sim_list,
          .f = ~ purrr::map(
            .x = tau_levels,
            .f = function(a) {
              PermFDATest::perm_test(
                alpha = a,
                realizations = .x$tau_vals,
                realized_value = .x$tau_real
              )
            }
          )
        )
      ), nrow = length(sim_list), ncol = length(tau_levels), byrow = TRUE
    )
  )
  colnames(res) <- paste0(
    "test_func_",
    c(paste0("alpha_tau=", tau_levels))
  )

  return(res)
}

test_decisions <- purrr::map(.x = simulation_results, .f = ~ val_test_func(.x))

# summarize rejection frequencies
rejection_frequencies <- function(test_tibble) {
  test_tibble %>%
    summarize(across(.cols = everything(), .fns = ~ mean(x = .x, na.rm = FALSE)))
}

summaries <- purrr::map(
  .x = test_decisions,
  .f = ~ rejection_frequencies(test_tibble = .x))

# give correct names to objects
names <- substr(x = individual_simulation_folders, start = 57, stop = 100)
names(summaries) <- names

# save summaries
saveRDS(object = summaries, file = paste0(output_path, 'Persistence_test2_rejection_frequencies.RDS'))
