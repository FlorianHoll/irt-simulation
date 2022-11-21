source("one_simulation_iteration.R")

library(foreach)
library(yaml)
library(doParallel)

# Set up parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1)
registerDoParallel(cl)

# read arguments from the command line
args <- commandArgs(TRUE)
filename <- as.double(args[1])
simulation_name <- as.double(args[2])

# read in the .yml file containing the simulation parameters
params <- yaml::read_yaml(filename)
parameters <- params[[simulation_name]]  # parameters are stored as a list

# start the parallelized for loop
final_results <- (
  foreach::foreach(
    i = 1:nr_iterations,
    .packages = c("mirt", "tidyverse")
  ) %dopar% {  # parallel processing
    do.call(simulate_one_iteration, parameters)
  }
)
stopCluster(cl)

# save simulation results to results folder.
dir.create("./results")
filename = paste0("./results/results_", simulation_name, ".Rds")
saveRDS(final_results, file = filename)
