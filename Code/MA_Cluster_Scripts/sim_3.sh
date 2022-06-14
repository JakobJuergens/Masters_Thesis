#!/bin/bash

#SBATCH --array=1-11%11

#SBATCH --cpus-per-task=1

#SBATCH --mem-per-cpu=4000M

#SBATCH --time=00:45:0

#SBATCH --account=ag_ifs_liebl

#SBATCH --job-name=MA_Simulation_3

#SBATCH --output=txt_outputs/output%A%a.txt

###beginning of executable commands

printf "Simulation 3 started. \n"

module load R
printf "R loaded \n"

now=$(date)
printf "Starting Time: %s\n" "$now"

Rscript simulation_3.R

now=$(date)
printf "Stopping Time: %s\n" "$now"