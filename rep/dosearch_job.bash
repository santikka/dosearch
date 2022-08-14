#!/bin/bash -l
#SBATCH --job-name=dosearch_array
#SBATCH --account=jkarvane
#SBATCH --output=dosearch_out_%j_%a.txt
#SBATCH --error=dosearch_err_%j_%a.txt
#SBATCH --partition=large
#SBATCH --time=32:00:00
#SBATCH --array=0-19
#SBATCH --ntasks=51
#SBATCH --mem-per-cpu=2000

# Load the R environment.
# r-env-singularity is a singularity container including 
# basic R-packages and operations specific to the used computer cluster.
# See https://docs.csc.fi/apps/r-env-singularity/.

module load r-env-singularity

# Clean up .Renviron file in home directory
if test -f  ~/.Renviron; then
  sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp directory path
echo "TMPDIR=/scratch/jkarvane" >> ~/.Renviron

# Specify arguments of the R script
# lib_path    : the installation directory of the dosearch package
# file_path   : the directory containing the simulation R script
# result_path : the directory to save the results in
# max_size    : maximum graph size (number of nodes)
lib_path=/projappl/jkarvane/r_packages_singularity
file_path=/users/stikka/dosearch
result_path=/users/stikka/dosearch/Results

max_size=10 #Adjust this if needed, 10 was used in the paper and takes around a day.

# Run the R script
srun singularity_wrapper exec Rscript --no-save --slave ${file_path}/simulation_large.R $lib_path $result_path $max_size $SLURM_ARRAY_TASK_ID
