library(mirt)
#' Simulate which items belong to which factor.
#'
#' This is done by sampling the factor that each item belongs 
#' to with the constraint that each factor must have at least
#' three items loading onto it. These are then converted to a 
#' list where each entry represents one factor.
#'
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
    factors = sample(nr_dimensions, nr_items, replace=TRUE)
    if (all(table(factors) >= 3)) break
  }
  
  factor_loadings <- lapply(1:nr_dimensions, 
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
#' @param nr_items 
#' @param nr_dimensions 
#'
#' @return
#'
#' @examples
simulate_alphas <- function(
  nr_items, nr_dimensions, factor_loadings
  ) {
  
  alphas <- matrix(0.0, nrow=nr_items, ncol=nr_dimensions+1)
  alphas[,1] <- rgamma(nr_items, shape=2, rate=1)
  
  for(i in 2:5) {
    f <- factor_loadings[[i-1]]
    alphas[f, i] <- rnorm(length(f))
  }
  
  return(alphas)
  
}

#' Title
#'
#' @param nr_items 
#' @param nr_dimensions 
#' @param factor_loadings 
#'
#' @return
#' @export
#'
#' @examples
create_model_syntax <- function(nr_items, nr_dimensions, factor_loadings) {
  
  model_syntax <- paste0("G=1-", nr_items)
  
  for(i in 1:nr_dimensions) {
    model_syntax <- paste0(
      model_syntax, "\nF", i, "=", str_flatten(
        factor_loadings[[i]], collapse = ","
      )
    )
  }
  
  return(mirt::mirt.model(model_syntax))
  
}
