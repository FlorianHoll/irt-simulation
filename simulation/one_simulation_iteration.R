library(mirt)
library(tidyverse)

source("utils.R")
source("multidimensional_irt.R")

#' One iteration of the IRT simulation.
#'
#' This means simulating theta, delta and alpha parameters, simulating the
#' responses, estimating a model and comparing the estimated coefficients
#' with the true coefficients.
#'
#' @param nr_persons The number of persons in one iteration of the simulation.
#'                   Defaults to 1000.
#' @param nr_items The number of items that the simulated test shall have.
#'                 Defaults to 20.
#' @param nr_dimensions The number of dimensions of the simulated model.
#' @param model_type The model type, i.e. "Rasch" or "2PL". Defaults to "2PL."
#' @param priors Boolean: Use priors on the alpha parameters? Defaults to TRUE.
#'
#' @return A matrix of size nr_items x nr_dimensions+1 representing
#'         the deviations of the estimated from the true parameters.
#'
#' @examples
simulate_one_iteration <- function(
    nr_persons = 1000,
    nr_items = 20,
    nr_dimensions = 3,
    model_type = "2PL",
    priors = TRUE
  ) {

  priors <- as.logical(priors)

  # Simulate person abilities (draw from normal distribution)
  thetas <- simulate_thetas(nr_persons, nr_dimensions)
  
  # Simulate item easiness parameters (draw from normal distribution)
  deltas <- rnorm(n = nr_items, mean = 0, sd = 1)
  deltas[deltas < -1.5] <- -1.5
  deltas[deltas > 1.5] <- 1.5
  deltas <- -deltas * rnorm(nr_items, 1, .2)

  # Simulate item discrimination parameters (draw from gamma distribution)
  factor_loadings <- simulate_factors(nr_items, nr_dimensions)
  alphas <- simulate_alphas(nr_items, nr_dimensions, factor_loadings)

  # Simulate the responses of the N persons.
  probabilities <- multidimensional_irt(alphas, deltas, thetas)
  simulated_data <- simulate_irt_data(probabilities)

  print(table(simulated_data))
  
  # Estimate the model.
  model_syntax <- create_model_syntax(
    nr_items, nr_dimensions, factor_loadings, priors
  )
  estimation_algorithm <- ifelse(nr_dimensions >= 3, "QMCEM", "EM")
  model <- mirt::mirt(
    data = as.data.frame(simulated_data),
    model = model_syntax,
    itemtype = model_type,
    method = estimation_algorithm
  )

  # Calculate differences between the estimated and true values.
  true_parameters <- cbind(alphas, deltas) %>% as.data.frame
  names(true_parameters) <- c(paste0("a", 1:(ncol(true_parameters) - 1)), "d")
  estimated_parameters <- (
    coef(model, simplify = T)$items %>%
      as.data.frame() %>%
      select(-g, -u)
  )
  deviation <- estimated_parameters - true_parameters

  # Store results as a list with parameters
  result <- list(
    params = list(
      nr_persons = nr_persons,
      nr_items = nr_items,
      nr_dimensions = nr_dimensions,
      model_type = model_type
    ),
    true_parameters = true_parameters,
    estimated_parameters = estimated_parameters,
    deviation = deviation
  )

  return(result)
}
