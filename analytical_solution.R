#' Analytical solution of the coupon collector's problem.
#'
#' @param n Number of different coupons.
#'
#' @return The expected number of coupons to draw in order to collect all
#' n coupons.
#'
#' @examples
#' analytical_solution(16)
analytical_solution <- function(n) {
  n * sum(1/1:n)
}
