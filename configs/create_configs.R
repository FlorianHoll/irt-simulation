library(yaml)

simulations <- expand.grid(
  nr_items=c(5, 10, 20, 40, 80),
  nr_dimensions=c(1, 2, 3, 5),
  nr_persons=c(30, 50, 100, 200, 500, 1000),
  model_type="2PL"
)
rownames(simulations) <- paste0("simulation", 1:nrow(simulations))
simulations_list <- simulations %>% 
  split(seq(nrow(simulations))) %>% 
  setNames(rownames(simulations))

write_yaml(simulations_list, "./configs/configs.yml")
