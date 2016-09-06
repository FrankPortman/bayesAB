#include <Rcpp.h>
using namespace Rcpp;

double lbeta2(double a, double b) {
  
  return lgamma(a) + lgamma(b) - lgamma(a + b);
  
}

// [[Rcpp::export]]
double bayesBernoulliTestClosed_(double alpha_1, double beta_1, double alpha_2, double beta_2) {
  
  double total = 0;
  
  for(int i = 0; i < alpha_2; i++) {
    
    total += exp(lbeta2(alpha_1 + i, beta_1 + beta_2) - log(beta_2 + i) - lbeta2(1 + i, beta_2) - lbeta2(alpha_1, beta_1));
    
  }
  
  return 1 - total;
  
}
