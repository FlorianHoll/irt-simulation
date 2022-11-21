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
#' @param nr_iterations The number of iterations of the simulation.
#' @param nr_persons The number of persons in one iteration of the simulation.
#'                   Defaults to 1000.
#' @param nr_items The number of items that the simulated test shall have.
#'                 Defaults to 20.
#' @param nr_dimensions The number of dimensions of the simulated model.
#' @param model_type The model type, i.e. "Rasch" or "2PL".
#'
#' @return A matrix of size nr_items x nr_dimensions+1 representing
#'         the deviations of the estimated from the true parameters.
#'
#' @examples
simulate_one_iteration <- function(nr_iterations = 1000,
                                   nr_persons = 1000,
                                   nr_items = 20,
                                   nr_dimensions = 3,
                                   model_type = "2PL") {
  # Simulate person abilities (draw from normal distribution)
  thetas <- (
    rnorm(n = nr_persons * nr_dimensions, mean = 0, sd = 2) %>%
      matrix(nrow = nr_persons)
  )

  # Simulate item easiness parameters (draw from normal distribution)
  deltas <- rnorm(n = nr_items, mean = 0, sd = 2)

  # Simulate item discrimination parameters (draw from gamma distribution)
  factor_loadings <- simulate_factors(nr_items, nr_dimensions)
  alphas <- simulate_alphas(nr_items, nr_dimensions, factor_loadings)

  # Simulate the responses of the N persons.
  probabilities <- multidimensional_irt(alphas, deltas, thetas)
  simulated_data <- simulate_irt_data(probabilities)

  # Estimate the model.
  model_syntax <- create_model_syntax(nr_items, nr_dimensions, factor_loadings)
  model <- mirt::mirt(
    model = model_syntax,
    itemtype = model_type
  )

  estimated_coefficients <- mirt::coef(model, as.data.frame = T)
  true_coefficients <- cbind(alphas, deltas)

  deviation <- as.data.frame(estimated_coefficients - true_coefficients)

  return(deviation)
}
