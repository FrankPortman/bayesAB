#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

double lbeta(double a, double b) {
  
  
  return lgamma(a) + lgamma(b) - lgamma(a + b);
  
}

// [[Rcpp::export]]
double bayesPoissonTestClosed_(double alpha_1, double beta_1, double alpha_2, double beta_2) {
  
  double total = 0;
  
  for(int i = 0; i < alpha_1; i++) {
    
    total += exp(i * log(beta_1) + alpha_2 * log(beta_2) - (i + alpha_2) * log(beta_1 + beta_2) - log(i + alpha_2) - lbeta(i + 1, alpha_2));
    
  }
  
  return total;
  
}
