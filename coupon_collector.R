#' Simulation of the coupon collector's problem.
#'
#' @param n Number of different coupons.
#' @param sims Number of simulations. 
#'
#' @return The expected number of coupons to draw in order to collect all
#' n coupons.
#' 
#' @examples
#' coupon_collector(16)
coupon_collector <- function(n, trials = 200L) {
  if (length(n) > 1 || n %% 1 != 0)
    stop("The parameter 'n' should be a single integer.", call. = FALSE)
  
  if (length(trials) > 1 || trials %% 1 != 0)
    stop("The parameter 'trials' should be a single integer.", call. = FALSE)
  
  needed_coupons <- rep(0, trials)
  
  for (trial in 1:trials) {
    collected_coupons <- c()
    n_collected_coupons <- 0
    coupon_set <- 1:n
    
    while (length(collected_coupons) < n) {
      coupon <- sample(coupon_set, 1)
      n_collected_coupons <- n_collected_coupons + 1
      
      if (!(coupon %in% collected_coupons)) 
        collected_coupons <- c(collected_coupons, coupon)
    }
    
    needed_coupons[[trial]] <- n_collected_coupons
  }  
  
  mean(needed_coupons)
}
