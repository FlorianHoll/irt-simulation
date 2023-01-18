library(yaml)
library(dplyr)

simulations <- expand.grid(
  nr_persons=c(200, 300),
  nr_items=40,
  nr_dimensions=c(1, 3, 4, 5),
  priors="TRUE",
  model_type="2PL"
)
rownames(simulations) <- paste0("simulation", 1:nrow(simulations))
simulations_list <- simulations %>% 
  split(seq(nrow(simulations))) %>% 
  setNames(rownames(simulations))

simulations_list <- lapply(simulations_list, 
                           function(x) {
                             list(params=x, nr_iterations=500)
                           }
)

write_yaml(simulations_list, "min_sample_40_items.yml")
