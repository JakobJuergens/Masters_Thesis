#!/bin/bash

#SBATCH --array=1-32%32

#SBATCH --cpus-per-task=1

#SBATCH --mem-per-cpu=4000M

#SBATCH --time=10:00:0

#SBATCH --account=ag_ifs_liebl

#SBATCH --job-name=MA_Simulation_persistence

#SBATCH --output=txt_outputs/output%A%a.txt

###beginning of executable commands

printf "Persistence Simulation started. \n"

module load R
printf "R loaded \n"

now=$(date)
printf "Starting Time: %s\n" "$now"

Rscript persistence_simulation.R

now=$(date)
printf "Stopping Time: %s\n" "$now"