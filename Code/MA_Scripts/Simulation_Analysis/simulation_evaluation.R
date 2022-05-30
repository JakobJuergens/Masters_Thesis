# load necessary packages
library(tidyverse)
library(stringr)
library(lubridate)

# define levels of the tests that are performed
tau_levels <- c(0.05, 0.04, 0.03, 0.025, 0.02, 0.01)
nu_levels <- c(0.05, 0.01, 0.02, 0.025, 0.03, 0.04)

# combination function
test_combine <- Vectorize(function(phi1, phi2){
  if(phi1 == 1 | phi2 == 1){return(1)}
  else if(phi1 == 0 | phi2 == 0){return(phi1 + phi2)}
  else{return(1 - (1-phi1)*(1-phi))}
})

# set path to simulation folders
sim_folders <- "../../Data/Cluster_Simulation_Outputs/"

# set individual folder paths
sim_1_path <- paste0(sim_folders, "Simulation_1/28_05_2022/")
sim_2_path <- paste0(sim_folders, "Simulation_2/30_05_2022/")
sim_3_path <- paste0(sim_folders, "Simulation_3/")
sim_4_path <- paste0(sim_folders, "Simulation_4/")
# set Output path
output_path <- "../../Data/Cluster_Simulation_Outputs/Evaluation/"

# read in simulation results
sim_1_files <- purrr::map(
  .x = list.files(path = sim_1_path),
  .f = ~ readRDS(paste0(sim_1_path, .x))
)

sim_2_files <- purrr::map(
  .x = list.files(path = sim_2_path),
  .f = ~ readRDS(paste0(sim_2_path, .x))
)

# extract realized values
sim_1_realized <- purrr::map(
  .x = sim_1_files,
  .f = ~ list(tau_real = .x$tau_real, nu_real = .x$nu_real)
)

sim_2_realized <- purrr::map(
  .x = sim_2_files,
  .f = ~ list(tau_real = .x$tau_real, nu_real = .x$nu_real)
)

# calculate critical values
sim_1_val_test_func <- cbind(
  # tau test decisions
  as_tibble(
    matrix(
      data = unlist(
        purrr::map(
          .x = sim_1_files,
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
      ), nrow = length(sim_1_files), ncol = length(tau_levels), byrow = TRUE
    )
  ),
  # nu test decisions
  as_tibble(
    matrix(
      data = unlist(
        purrr::map(
          .x = sim_1_files,
          .f = ~ purrr::map(
            .x = nu_levels,
            .f = function(a) {
              PermFDATest::perm_test(
                alpha = a,
                realizations = .x$nu_vals,
                realized_value = .x$nu_real
              )
            }
          )
        )
      ), nrow = length(sim_1_files), ncol = length(nu_levels), byrow = TRUE
    )
  )
)
colnames(sim_1_val_test_func) <- paste0(
  "test_func_",
  c(paste0("alpha_tau=", tau_levels), paste0("alpha_nu=", nu_levels))
)

sim_2_val_test_func <- cbind(
  # tau test decisions
  as_tibble(
    matrix(
      data = unlist(
        purrr::map(
          .x = sim_2_files,
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
      ), nrow = length(sim_2_files), ncol = length(tau_levels), byrow = TRUE
    )
  ),
  # nu test decisions
  as_tibble(
    matrix(
      data = unlist(
        purrr::map(
          .x = sim_2_files,
          .f = ~ purrr::map(
            .x = nu_levels,
            .f = function(a) {
              PermFDATest::perm_test(
                alpha = a,
                realizations = .x$nu_vals,
                realized_value = .x$nu_real
              )
            }
          )
        )
      ), nrow = length(sim_2_files), ncol = length(nu_levels), byrow = TRUE
    )
  )
)
colnames(sim_2_val_test_func) <- paste0(
  "test_func_",
  c(paste0("alpha_tau=", tau_levels), paste0("alpha_nu=", nu_levels))
)

# find combined test decisions
sim_1_test_combined <- sim_1_val_test_func %>%
  mutate(comb_40_10 = test_combine(`test_func_alpha_tau=0.04`, `test_func_alpha_nu=0.01`),
         comb_30_20 = test_combine(`test_func_alpha_tau=0.03`, `test_func_alpha_nu=0.02`),
         comb_25_25 = test_combine(`test_func_alpha_tau=0.025`, `test_func_alpha_nu=0.025`),
         comb_20_30 = test_combine(`test_func_alpha_tau=0.02`, `test_func_alpha_nu=0.03`),
         comb_10_40 = test_combine(`test_func_alpha_tau=0.01`, `test_func_alpha_nu=0.04`)) %>%
  select(`test_func_alpha_tau=0.05`, `test_func_alpha_nu=0.05`, starts_with('comb_'))

sim_2_test_combined <- sim_2_val_test_func %>%
  mutate(comb_40_10 = test_combine(`test_func_alpha_tau=0.04`, `test_func_alpha_nu=0.01`),
         comb_30_20 = test_combine(`test_func_alpha_tau=0.03`, `test_func_alpha_nu=0.02`),
         comb_25_25 = test_combine(`test_func_alpha_tau=0.025`, `test_func_alpha_nu=0.025`),
         comb_20_30 = test_combine(`test_func_alpha_tau=0.02`, `test_func_alpha_nu=0.03`),
         comb_10_40 = test_combine(`test_func_alpha_tau=0.01`, `test_func_alpha_nu=0.04`)) %>%
  select(`test_func_alpha_tau=0.05`, `test_func_alpha_nu=0.05`, starts_with('comb_'))

# summarize rejection frequencies
sim_1_rejection_frequencies <- sim_1_test_combined %>%
  summarize(across(.cols = everything(), .fns = ~ mean(x = .x, na.rm = FALSE)))

sim_2_rejection_frequencies <- sim_2_test_combined %>%
  summarize(across(.cols = everything(), .fns = ~ mean(x = .x, na.rm = FALSE)))

# save outputs to designated folder
saveRDS(sim_1_test_combined, paste0(output_path, 'sim_1_test_functions.RDS'))
saveRDS(sim_1_rejection_frequencies, paste0(output_path, 'sim_1_rejection_frequencies.RDS'))

saveRDS(sim_2_test_combined, paste0(output_path, 'sim_2_test_functions.RDS'))
saveRDS(sim_2_rejection_frequencies, paste0(output_path, 'sim_2_rejection_frequencies.RDS'))
