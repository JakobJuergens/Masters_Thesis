#!/bin/bash

#SBATCH --array=1-4%4

#SBATCH --cpus-per-task=1

#SBATCH --mem-per-cpu=4000M

#SBATCH --time=0:40:0

#SBATCH --account=ag_ifs_liebl

#SBATCH --job-name=MA_Simulation_1

#SBATCH --output=txt_outputs/output%A%a.txt

###beginning of executable commands

printf "Simulation 1 started. \n"

module load R
printf "R loaded \n"

now=$(date)
printf "Starting Time: %s\n" "$now"

Rscript simulation_1.R

now=$(date)
printf "Stopping Time: %s\n" "$now"
