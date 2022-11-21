library(mirt)
library(dplyr)

#' Simulate which items belong to which factor.
#'
#' This is done by sampling the factor that each item belongs
#' to with the constraint that each factor must have at least
#' three items loading onto it. These are then converted to a
#' list where each entry represents one factor.
#'
#' @param nr_items The number of items.
#' @param nr_dimensions The number of factors/ dimensions.
#'
#' @return The factor loadings as a list where each entry
#'         represents one factor and each number within the
#'         entry represents the item number.
#'
#' @examples
simulate_factors <- function(nr_items, nr_dimensions) {
  while (TRUE) {
    factors <- sample(nr_dimensions, nr_items, replace = TRUE)
    if (all(table(factors) >= 3)) break
  }

  factor_loadings <- lapply(
    1:nr_dimensions,
    function(factor) {
      which(factors == factor)
    }
  )

  return(factor_loadings)
}

#' Simulate alpha parameters while taking the factor loadings into account.
#'
#' An item has a loading of 0 onto a factor whenever it is not part of that
#' factor.
#'
#'
#'
#'
#'
#' @param nr_items The number of items.
#' @param nr_dimensions The number of dimensions.
#' @param factor_loadings The factor loadings (a list with each index 
#'                        containing the item indezes for each factor 
#'                        respectively).
#'
#' @return A matrix containing the alpha loadings for all dimensions/ factors
#'         where each row represents an item and each column represents a
#'         factor. The first factor always represents the so-called
#'         "G factor" - the factor that all items load onto.
#'
#' @examples
simulate_alphas <- function(nr_items, nr_dimensions, factor_loadings) {

  if (nr_dimensions == 1) {
    # If there is only one dimension (a unidimensional model), there is only
    # the G factor. Therefore, the alphas are a matrix with only one column.
    alphas <- rgamma(nr_items, shape = 2, rate = 1) %>%
      matrix(nrow = nr_items, ncol = 1)

  } else {
    # If there are more than one dimension (a multidimensional model), there
    # will always be one column corresponding to the G factor that all items
    # load onto. Additionally, there will be columns corresponding to the
    # sub-factors that a subset of the items load onto. This means that for
    # example for nr_dimensions=3, the alphas will be a matrix with dimensions
    # nr_items x 4.
    alphas <- matrix(0.0, nrow = nr_items, ncol = nr_dimensions + 1)
    alphas[, 1] <- rgamma(nr_items, shape = 2, rate = 1)

    for (i in 2:(nr_dimensions+1)) {
      idz <- factor_loadings[[i - 1]]
      alphas[idz, i] <- rgamma(length(idz), shape = 2, rate = 1)
    }
  }

  return(alphas)
}

#' Create the model syntax to be passed to the mirt::mirt command.
#'
#' The model syntax has to contain the factor loadings and covariance
#' structure of the model.
#'
#' @param nr_items The number of items.
#' @param nr_dimensions The number of dimensions
#' @param factor_loadings The factor loadings (a list with each index 
#'                        containing the item indezes for each factor 
#'                        respectively).
#' @param priors Boolean: Set priors on the alpha parameters? Defaults to True.
#'
#' @return The model syntax as a mirt::mirt.model object, ready to be 
#'         passed to the mirt::mirt command.
#'
#' @examples
create_model_syntax <- function(
    nr_items, nr_dimensions, factor_loadings, priors = TRUE
) {
  model_syntax <- paste0("G=1-", nr_items)

  if (nr_dimensions > 1) {
    for (i in 1:nr_dimensions) {
      model_syntax <- paste0(
        model_syntax, "\nF", i, "=", str_flatten(
          factor_loadings[[i]],
          collapse = ","
        )
      )
    }
  }
  
  if (priors) {
    # Add priors to the model syntax.
    model_syntax <- (
      paste0(model_syntax, "\nPRIOR=(1-", nr_items, ", a1, lnorm, 1, 1)")
    )
    
    # Factor-specific priors.
    if (nr_dimensions > 1) {
      for (i in 1:nr_dimensions) {
        model_syntax <- paste0(
          model_syntax, ", (", 
          str_flatten(
            factor_loadings[[i]],
            collapse = ","
          ),
          ", a",
          i+1,
          ", lnorm, 1, 1)"
        )
      }
    }
  }
  
  return(mirt::mirt.model(model_syntax))
}
