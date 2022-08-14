# Introduction
This directory contains the replication materials reproducing the results for the article
"Causal Effect Identification from Multiple Incomplete Data Sources: A General Search-based Approach"
by Santtu Tikka, Antti Hyttinen and Juha Karvanen, published in the Journal of Statistical Software.

# Contents
The following files are included:
 - dosearch_job.bash:     bash script that submits a SLURM array job to conduct the large-scale simulation
 - replication.R:         R-script for reproducing all results of the paper
 - replication.html       An html report obtained by running knitr::spin() on replication.R
 - Results.tar.gz         Simulation results from the large-scale simulation
 - simulation_large.R:    R-script for the large-scale simulation
 - simulation_small.R:    R-script for the small-scale simulation

# Usage
To replicate all results of the paper, the simulation has to be run first.
We recommend running the small-scale simulation that produces similar results
as shown in the paper with graphs that have fewer nodes. 


# Small scale simulations
#########################

This should take around 10 minutes. The results are produced automatically via the replication.R script:

> setwd("~/dosearch/rep") #set working directory accordingly
> source('replication.R') #source the script


# Large scale simulations
########################

These simulations need a computer cluster with MPI and job scheduling capabilities through slurm. The running time is around a day.

(The steps 1-4 perform the large scale computation. The produced results are in Results.tar.gz, by unzipping you can skip the computation.)

To replicate the large-scale simulation, follow these steps:

  1. Install the dosearch R-package (If necessary, use a custom library path via install.packages("dosearch_1.0.7.tar.gz", lib = lib_path),
     where lib_path is a directory where R packages can be stored and installed).

  2. Modify the parameters of the SLURM bash script (dosearch_job.bash) to adapt to your computational environment.

    - If your computational environment cannot accommodate the default values in dosearch_job.bash (an array of 20 jobs,
      each with a time limit of 32 hours, 51 cores, and 2Gb of reserved memory) due to any potential restrictions, 
      change them if necessary (total number of instances will always be array size times the number of cores - 1).
      
    - The bash script uses a cluster specific singularity container "r-env-singularity" including basic R packages (and MPI). 
      Adapt this according to your environment.

    - Set the lib_path variable in dosearch_job.bash such that it matches the directory where dosearch was installed in step 1.

    - Set the file_path variable in dosearch_job.bash such that it points to the location of simulation_large.R script.

    - Set the result_path variable in dosearch_job.bash to a directory where the simulation results should be saved as .RData files. 
      This directory should be "*/rep/Results", make sure it exists. 

    - If necessary, change the variable max_size in dosearch_job.bash to a smaller value (default value is 10 for graphs with 10 vertices
      as presented in the paper.)

  3. Submit the job and wait for completion:

    $ sbatch dosearch_job.bash

  4. After completion, the following files should be produced in the result_path directory as defined above: 

    $ ls rep/Results/
    dosearch_simulation_results_0.RData   dosearch_simulation_results_13.RData  dosearch_simulation_results_18.RData  dosearch_simulation_results_5.RData
    dosearch_simulation_results_1.RData   dosearch_simulation_results_14.RData  dosearch_simulation_results_19.RData  dosearch_simulation_results_6.RData
    dosearch_simulation_results_10.RData  dosearch_simulation_results_15.RData  dosearch_simulation_results_2.RData   dosearch_simulation_results_7.RData
    dosearch_simulation_results_11.RData  dosearch_simulation_results_16.RData  dosearch_simulation_results_3.RData   dosearch_simulation_results_8.RData
    dosearch_simulation_results_12.RData  dosearch_simulation_results_17.RData  dosearch_simulation_results_4.RData   dosearch_simulation_results_9.RData

  5. Finally, run the replication.R script to replicate all results of the paper (or run knit::spin on the script).
     The script assumes that the working directory is the location of the script itself. 

    > setwd("~/dosearch/rep") # set working directory accordingly
    > source('replication.R') # source the script

    The following pdf figures will be produced as .pdf files in the "Results" directory.

    $ ls rep/Results/*.pdf
    rep/Results/scatter_id_h.pdf   rep/Results/scatter_id_i.pdf     rep/Results/scatter_nonid_hi.pdf  rep/Results/time_by_n.pdf
    rep/Results/scatter_id_hi.pdf  rep/Results/scatter_nonid_h.pdf  rep/Results/scatter_nonid_i.pdf

    These are directly the plots included in the paper in Figures 4, 5 and 6.

    Note that the small-scale simulation will be replicated automatically via replication.R (via sourcing simulation_small.R) if no pre-existing 
    simulation results are present in the "rep/Results" directory.
