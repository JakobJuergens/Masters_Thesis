#!/bin/bash

#SBATCH --array=1-2%2

#SBATCH --cpus-per-task=48

#SBATCH --mem-per-cpu=3800M

#SBATCH --time=100

#SBATCH --job-name=MA_Simulation_3

#SBATCH --output=txt_outputs/output%A%a.txt

###beginning of executable commands

Rscript simulation_3.R