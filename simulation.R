source("./simulation/one_simulation_iteration.R", chdir = TRUE)

library(foreach)
library(yaml)
library(doParallel)

# Set up parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1] - 1)  # leave one core available
registerDoParallel(cl)

# read arguments from the command line
args <- commandArgs(TRUE)
filename <- args[1]
run_from <- args[2]
run_to <- args[3]

# read in the .yml file containing the simulation parameters
cat(paste("Reading", filename, "as the config file."))
params <- yaml::read_yaml(file = filename)

run_from <- ifelse(is.na(run_from), 1, run_from)
run_to <- ifelse(is.na(run_to), length(params), run_to)

results_folder = paste0(
  "./results/", gsub(":", "-", as.character(Sys.time()))
)

# start simulations in a parallelized for loop
if (
  !(foreach::getDoParRegistered()) | (foreach::getDoParWorkers() <= 1)
) {
  warning("Only one CPU node is used.")
}

# iterate through all simulations in the .yml file.
for (simulation in names(params)[run_from : run_to]) {

  nr_iterations <- params[[simulation]][["nr_iterations"]]
  parameters <- params[[simulation]][["params"]]

  # Ensure that dimensions can be built with the number of items.
  if (parameters$nr_items >= (3 * parameters$nr_dimensions)) {
    
    cat(
      paste(
        "\n\nRunning", simulation, "with", nr_iterations, 
        "iterations and the following parameters:\n"
      )
    )
    cat(paste0(names(parameters), ": ", parameters, "; "))

    # run simulation
    final_results <- (
      foreach::foreach(
        i = 1:nr_iterations,
        .packages = c("mirt", "tidyverse")
      ) %dopar% {  # parallel processing
        do.call(simulate_one_iteration, parameters)
      }
    )
    
    # save simulation results to results folder.
    dir.create(results_folder)
    filename <- paste0(results_folder, "/results_", simulation, ".Rds")
    saveRDS(final_results, file = filename)
  }
  
}
