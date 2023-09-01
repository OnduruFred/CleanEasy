#' Systematic Sample function
#'
#' Function allows you to sample data based on Systematic sample.
#'
#'
#' @export
#' @param  N - The data set to be operated on
#' @param  n - sample size


# ---   Systematic Sampling

syst_sampling = function(N,n){
  set.seed(123)
  k = ceiling(N/n)
  r = sample(1:k, 1)
  seq(r, r + k*(n-1), k)
}
