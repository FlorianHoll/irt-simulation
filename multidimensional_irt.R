library(dplyr)

#' Multidimensional Item Response Theory probability modeling.
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
#' @param betas The betas, i.e. the difficulty parameters of the items.
#'              Vector of size 1 x M.
#' @param thetas The thetas, i.e. the person abilties. Matrix of size N x D.
#'
#' @return A matrix of size N x M representing the probabilities of the persons
#'         solving the items correctly.
#'
#' @examples 
multidimensional_irt <- function(
    alphas, betas, thetas
  ) {
  
  # Shapes
  N = nrow(thetas)
  D = nrow(alphas)
  M = ncol(alphas)
  
  # Convert betas to matrix so that matrix addition works
  betas = betas %>% rep(each=N) %>% matrix(nrow=N)
  
  # IRT formula
  probability_matrix = 1 / (1 + exp(-(thetas %*% alphas + betas)))
  
  return(probability_matrix)
}


