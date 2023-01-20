library(tidyverse)

#' Get the deviations of one single simulation iteration.
#'
#' @param result_sublist The single simulation iteration results.
#' @param what_to_get String that selects columns to retrieve. Can either
#'                    be either "a" for discrimination parameters or "d"
#'                    for difficulty parameters.
#' @param only_g_factor Only applicable if what_to_get == "a"; indicates
#'                      whether to only get the G factor or to get all 
#'                      factors.
#'
#' @return The flattened deviations as a vector.
get_deviations <- function(result_sublist, what_to_get = "a", only_g_factor) {
  
  result_sublist <- result_sublist$deviation %>%
    as.data.frame %>%
    select(starts_with(what_to_get))

  if ((what_to_get == "a") & (only_g_factor)) {
    return (result_sublist[,1])  # the G factor is always the first column
  }

  return (result_sublist %>% flatten %>% as.numeric)
}

#' Get the alpha, i.e. discrimination, deviations from the resulting list.
#'
#' @param result_list The list storing as contents all individual simulations
#'                    for a specific configuration of parameters.
#' @param only_g_factor Indicator: Only get the G factor or get the deviation
#'                      for all factors?
#'
#' @return A dataframe with each row corresponding to one parameter's deviation.
get_alpha_deviations <- function(result_list, only_g_factor = TRUE) {
  
  discrimination_deviations <- (
    sapply(result_list, function(x) get_deviations(x, "a", only_g_factor)) %>% as.numeric
  )
  
  model_results <- data.frame(
    discrimination_deviations = discrimination_deviations,
    nr_persons = result_list[[1]]$params$nr_persons,
    nr_items = result_list[[1]]$params$nr_items,
    nr_dimensions = result_list[[1]]$params$nr_dimensions,
    model_type = result_list[[1]]$params$model_type
  )
 
  return(model_results)
}

#' Get the beta, i.e. difficulty parameter, deviations from the resulting list.
#'
#' @param result_list The list storing as contents all individual simulations
#'                    for a specific configuration of parameters.
#'
#' @return A dataframe with each row corresponding to one parameter's deviation.
get_beta_deviations <- function(result_list) {
  
  difficulty_deviations <- (
    sapply(result_list, function(x) get_deviations(x, "d", FALSE)) %>% as.numeric
  )
  
  results <- data.frame(
    difficulty_deviations = difficulty_deviations,
    nr_persons = result_list[[1]]$params$nr_persons,
    nr_items = result_list[[1]]$params$nr_items,
    nr_dimensions = result_list[[1]]$params$nr_dimensions,
    model_type = result_list[[1]]$params$model_type
  )
  
  return (results)
  
}


#' Get the theta, i.e. person ability parameters, deviations from the list.
#'
#' @param result_list The list storing as contents all individual simulations
#'                    for a specific configuration of parameters.
#' @param only_g_factor Indicator: Only get the G factor or get the deviation
#'                      for all factors?
#'
#' @return A dataframe with each row corresponding to one parameter's deviation.
get_theta_deviations <- function(result_list, only_g_factor = TRUE) {

  if (only_g_factor) {
    theta_deviations <- (
      # The G factor is always the first column
      sapply(result_list, function(x) x$thetas_deviation[,1]) %>% as.numeric
    )
  } else {
    theta_deviations <- (
      sapply(result_list, function(x) x$thetas_deviation) %>% as.numeric
    )
  }
  
  person_results <- data.frame(
    theta_deviations = theta_deviations,
    nr_persons = result_list[[1]]$params$nr_persons,
    nr_items = result_list[[1]]$params$nr_items,
    nr_dimensions = result_list[[1]]$params$nr_dimensions,
    model_type = result_list[[1]]$params$model_type
  )
  
  return(person_results)
}

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))
