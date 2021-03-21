#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getCorr(NumericVector denom_X,NumericMatrix X,NumericVector sX,
                      int L,int h,NumericVector Ycounter){
  NumericVector rvec;
  Ycounter = Ycounter - 1;
  int prevCounter = 0;
  for(int i = 0;i<Ycounter.length();i++){
    int counter = Ycounter[i];
    for(int Xcounter = prevCounter;Xcounter<counter;Xcounter++){
      double den_X = (double)denom_X[Xcounter];
      double den_Y = (double)denom_X[counter];
      double den = std::sqrt(den_X * den_Y);
      double num_left = h * sum(X(Xcounter,_) * X(counter,_));
      double num_right = sX[Xcounter] * sX[counter];
      double num = num_left - num_right;
      double r = num / den;
      rvec.push_back(r);
    }
    prevCounter = counter + 1;
  }
  return rvec;
}

// [[Rcpp::export]]
double getSum(NumericVector x, NumericVector y){
  return sum(x * y);
}

/*** R

*/
