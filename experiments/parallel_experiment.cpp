#include <Rcpp.h>

using namespace Rcpp;

#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

std::vector<double> sY;
std::vector<double> sY2;

// [[Rcpp::export]]
void make_sY(NumericMatrix m) {
  for(int i = 0;i<m.nrow();i++){
    NumericVector vi = m(i,_);
    Rcout << vi << std::endl;
    vi = vi[Range(vi.size() - 2,vi.size())];
    sY.push_back(std::accumulate(vi.begin(),vi.end(),0));
  }
}

// [[Rcpp::export]]
void printsY(){
  for(int i = 0;i<sY.size();i++){
    Rcout << sY[i] << std::endl;
  }
}


/*** R
m <- matrix(c(1:64),nrow = 8,ncol = 8,byrow = TRUE)
make_sY(m)
printsY()
*/
