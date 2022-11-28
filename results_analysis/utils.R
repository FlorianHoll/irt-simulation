library(tidyverse)

get_deviations <- function(x, what_to_get = "a") {
  x$deviation %>%
    as.data.frame %>%
    select(starts_with(what_to_get)) %>%
    flatten %>%
    as.numeric
}

get_all_deviations <- function(result_list) {
  discrimination_deviations <- (
    sapply(result_list, function(x) get_deviations(x, "a")) %>% as.numeric
  )
  difficulty_deviations <- (
    sapply(result_list, function(x) get_deviations(x, "d")) %>% as.numeric
  )
  
  results <- data.frame(
    discrimination_deviations = discrimination_deviations,
    difficulty_deviations = difficulty_deviations,
    nr_persons = result_list[[1]]$params$nr_persons,
    nr_items = result_list[[1]]$params$nr_items,
    nr_dimensions = result_list[[1]]$params$nr_dimensions,
    model_type = result_list[[1]]$params$model_type
  )

  return(results)
}

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))
