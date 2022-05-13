#!/usr/local_rwth/bin/zsh

#SBATCH --array=1-2%2

#SBATCH --cpus-per-task=48

#SBATCH --mem-per-cpu=3800M

#SBATCH --time=100

#SBATCH --account=prep0031

#SBATCH --job-name=Sim_Test

#SBATCH --output=txt_outputs/output%A%a.txt

###beginning of executable commands

Rscript simulation_1.R