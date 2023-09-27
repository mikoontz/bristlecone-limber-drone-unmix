#' @description 
#' Wrapper for estimating Matthew's correlation coefficient with an arbitrary
#' number of dimension.
#' See second definition in section "multiclass case":
#' https://en.wikipedia.org/wiki/Phi_coefficient
#' 
#' @param m
#' Confusion matrix
#' 
#' @returns r_k, the Matthew's correlation coefficient for k classes.

r_k_fun <- function(m) {
  # m is a matrix
  
  # Correct classifications
  c <- sum(diag(m))
  # Total number of data points
  n <- sum(m)
  # Observed number of data points in each class
  o <- rowSums(m)
  # Predicted number of data points in each class
  p <- colSums(m)
  
  r_k <- (c*n - sum(o*p)) / (sqrt(n^2 - sum(o^2)) * sqrt(n^2 - sum(p^2)))
  # (see second definition in "multiclass case"
  # https://en.wikipedia.org/wiki/Phi_coefficient)
  
  return(r_k)
  
}
