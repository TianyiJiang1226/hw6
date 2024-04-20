#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector SimpLinCpp(NumericVector X, NumericVector Y) {
  int n = X.size();
  double mX = mean(X);
  double mY = mean(Y);
  double sXY = 0;
  double sXX = 0;
  for (int i = 0; i < n; i++) {
    sXY += (X[i] - mX) * (Y[i] - mY);
    sXX += (X[i] - mX) * (X[i] - mX);
  }

  double beta1 = sXY / sXX;
  double beta0 = mY - beta1 * mX;

  NumericVector beta = NumericVector::create(_["beta0"] = beta0, _["beta1"] = beta1);
  return(beta);
}




// [[Rcpp::export]]
NumericVector boot_res(NumericVector fit, NumericVector resid, NumericVector x, int n){
  NumericMatrix res(n, 2);
  NumericVector p(40,0.025);
  for (int i = 0; i < n; i++) {
    NumericVector samps = sample(resid, 40, true,p);
    NumericVector newy = fit + samps;
    NumericVector beta = SimpLinCpp(x,newy);
    res(i,0) = beta(0);
    res(i,1) = beta(1);
  }
  return(res);
}


// [[Rcpp::export]]
NumericVector boot_case(NumericVector y, NumericVector x, int n){
  NumericMatrix res(n, 2);
  NumericVector p(40,0.025);
  Function seq("seq");
  NumericVector index = seq(0,39);
  for (int i = 0; i < n; i++) {
    NumericVector samps = sample(index, 40, true,p);
    NumericVector newy = y[samps];
    NumericVector newx = x[samps];
    NumericVector beta = SimpLinCpp(newx,newy);
    res(i,0) = beta(0);
    res(i,1) = beta(1);
  }
  return(res);
}
