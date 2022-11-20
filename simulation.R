library(mirt)
library(tidyverse)

#' IRT simulation.
#'
#'
#' @param nr_iterations The number of iterations of the simulation. Defaults to 1000.
#' @param nr_persons The number of persons in one iteration of the simulation.
#'                   Defaults to 1000.
#' @param nr_items The number of items that the simulated test shall have.
#'                 Defaults to 20.
#' @param nr_dimensions The number of dimensions of the simulated model.
#'
#' @return A matrix of size N x M representing the probabilities of the persons
#'         solving the items correctly.
#'
#' @examples 
simulate_irt <- function(
    nr_iterations=1000, nr_persons=1000, nr_items=20, nr_dimensions=3, model='2PL'
  ) {
  
  results = list()

  # Simulate person abilities (draw from normal distribution)
  thetas = (
    rnorm(n=nr_persons*nr_dimensions, mean=0, sd=2) %>% 
      matrix(nrow=nr_persons)
  )

  # Simulate item easiness parameters (draw from normal distribution)
  deltas = rnorm(n=nr_items, mean=0, sd=2)

  # Simulate item discrimination parameters (draw from gamma distribution)
  factors = sample(nr_dimensions, nr_items, replace=TRUE)
  
  
  alphas = (
    rgamma(n=nr_items*nr_dimensions, shape=2, rate=1) %>% 
      matrix(ncol=nr_dimensions)
  )

  probabilities = multidimensional_irt(alphas, deltas, thetas)
  simulated_data = simulate_irt_data(probabilities)
  model = mirt::mirt(
    paste0(
        'G = 1-',
        nr_items,
        'F1 = 1-'
    ),
    model=model
  )
  estimated_coefficients = mirt::coef(model, as.data.frame=T)
  true_coefficients = cbind(alphas, deltas)

  deviation = as.data.frame(estimated_coefficients - true_coefficients)

  return(deviation)

}


