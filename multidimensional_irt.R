library(dplyr)

#' Vectorized multidimensional Item Response Theory probability modeling.
#'
#' This function can handle uni- and multidimensional IRT models and 
#' returns a matrix of probabilities where each entry represents the
#' probability of one particular person to solve one item.
#' Let N be the number of persons, M be the number of items and 
#' D be the number of dimensions, then the resulting matrix is of 
#' size N x M.
#'
#' @param alphas The alphas, i.e. the discrimination parameters in the model.
#'               Matrix of size D x M.
#' @param deltas The deltas, i.e. the easiness parameters of the items.
#'              Vector of size M x 1.
#' @param thetas The thetas, i.e. the person abilties. Matrix of size N x D.
#'
#' @return A matrix of size N x M representing the probabilities of the persons
#'         solving the items correctly.
#'
#' @examples 
multidimensional_irt <- function(
    alphas, deltas, thetas
  ) {
  
  # Shapes
  N = nrow(thetas)
  D = nrow(alphas)
  M = ncol(alphas)
  
  # Convert betas to matrix so that matrix addition works
  deltas = t(deltas) %>% rep(each=N) %>% matrix(nrow=N)
  
  # IRT formula
  probability_matrix = 1 / (1 + exp(-((thetas %*% t(alphas)) + deltas)))
  
  return(probability_matrix)
}


#' Vectorized simulation of IRT responses based on a probability matrix.
#'
#' The function will simulate responses of participants given their 
#' probability to solve the item. Since this probability is simply assumed
#' to be a draw from a binomial distribution, simulating the responses is
#' simply the result of a draw from a binomial distribution with size 1.
#'
#' @param probability_matrix A matrix of size N x M, where N is the number of
#'                           participants and M is the number of items.
#'
#' @return The simulated responses; a matrix of shape N x M.
#'
#' @examples
simulate_irt_data <- function(probability_matrix) {
  
  N = dim(probability_matrix)[1]
  M = dim(probability_matrix)[2]
  
  results <- rbinom(n=N*M, size=1, prob=probability_matrix) %>% matrix(ncol=M)
  return(results)
}
