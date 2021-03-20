#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix split_list(List lstx,int h,int L){
  NumericMatrix mat(L,h);
  int counter = 0;
  for(int i = 0;i < lstx.size();i++){
    NumericVector v = lstx[i];
    for(int j = 0;j <= v.size() - h;j++){
      mat(counter,_) = v[Range(j,j+h)];
      counter = counter + 1;
    }
  }
  return mat;
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
