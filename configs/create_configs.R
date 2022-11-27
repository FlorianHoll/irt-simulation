library(yaml)
library(dplyr)

simulations <- expand.grid(
  nr_persons=c(50, 100, 200, 500, 1000),
  nr_items=c(5, 10, 20, 40, 80),
  nr_dimensions=c(1, 2, 3),
  priors=c("TRUE", "FALSE"),
  model_type="2PL"
)
rownames(simulations) <- paste0("simulation", 1:nrow(simulations))
simulations_list <- simulations %>% 
  split(seq(nrow(simulations))) %>% 
  setNames(rownames(simulations))

simulations_list <- lapply(simulations_list, 
                           function(x) {
                             list(params=x, nr_iterations=40)
                           }
)

write_yaml(simulations_list, "configs.yml")
