#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double coupon_collectorC(int n, int sims) {
  if (std::floor(n) != n)
    stop("The parameter 'n' should be a single integer.");
  
  if (std::floor(sims) != sims)
    stop("The parameter 'sims' should be a single integer.");
  
  NumericVector needed_coupons(sims);
  
  for (int trial = 0; trial < sims; trial++) {
    NumericVector collected_coupons(n);
    IntegerVector coupon_set = seq(1,n);
    int n_collected_coupons = 0;
    int n_different_coupons = 0;
    
    while (n_different_coupons < n) {
      int coupon = sample(coupon_set, 1)[0];
      n_collected_coupons++;
      
      if (std::find(collected_coupons.begin(), collected_coupons.end(), coupon) == collected_coupons.end()) {
        collected_coupons[n_different_coupons] = coupon;
        n_different_coupons++;
      }
      
    }
    
    needed_coupons[trial] = n_collected_coupons;
  }
  
  return mean(needed_coupons);
}
