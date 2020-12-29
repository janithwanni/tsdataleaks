#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix find_dataleaks_cpp(List l,int  h,double cutoff = 1){
  NumericMatrix out(2,3);
  int m = l.size();
  //make sY,sY2
  NumericVector sY = NumericVector(m);
  NumericVector sYsqrd = NumericVector(m);
  int max_ni = 0;
  for(int i = 0;i<m;i++){
    NumericVector v = l[i];
    if(v.size() > max_ni){
      max_ni = v.size();
    }
  }
  //make sX,sX2
  NumericMatrix sX = NumericMatrix(m,max_ni);
  NumericMatrix sXsqrd = NumericMatrix(m,max_ni);

  for(int i = 0;i<m;i++){
    NumericVector v = l[i];
    int ni = v.size();

    //calc sY,sY^2
    double sYi = 0;
    double sYsqrdi = 0;
    for(int j = 0;j<h;j++){
      //Rprintf("adding %dth element of %dth series = %.2f\n",(j+ni-h),i,v[j+ni-h]);
      sYi += v[j + ni - h];
      sYsqrdi += v[j + ni - h] * v[j + ni - h];
    }
    //Rprintf("%d %.2f \n",i,sYi);
    sY[i] = sYi;
    sYsqrd[i] = sYsqrdi;

    //calc sX,sX^2
    double sXi = 0;
    double sXsqrdi = 0;
    for(int j = 0;j<ni;j++){
      if (j < h){
        sXi += v[j];
        sXsqrdi += v[j] * v[j];
        sX(i,0) = sXi;
        sXsqrd(i,0)= sXsqrdi;
      }else{
        double prev = sX(i,j-h);
        double prevSqrd = sXsqrd(i,j-h);
        sX(i,j-h+1) = prev - v[j - h] + v[j];
        sXsqrd(i,j-h+1) = prevSqrd - (v[j-h]*v[j-h]) + (v[j] * v[j]);
      }
    }
  }
  // for(int i = 0;i<m;i++){
  //   NumericVector v = l[i];
  //   int ni = v.size();
  //   for(int j = 0;j<(ni - h);j++){
  //     Rprintf("%.2f ",sX(i,j));
  //   }
  //   Rprintf("\n");
  // }


  //calculate rWs

  for(int i = 0;i < m;i++){
    //NumericMatrix ends_i = NumericMatrix(m,max_ni);
    NumericVector vi = l[i];
    int ni = vi.size();
    double sYi = sY[i];
    double sYsqrdi = sYsqrd[i];
    // for(int j = 0;j < m;j++){
    //   NumericVector vj = l[j];
    //   int nj = vj.size();
    //   NumericVector ends = NumericVector(nj);
    //   ends = ends - 1;
    //   int cutoffCounter = 0;
    //   for(int w = 0;w<(nj-h+1);w++){
    //     double sXj = sX(j,w);
    //     double sXsqrdj = sXsqrd(j,w);
    //     double sXsY = 0;
    //     for(int k = w;k<w+h;k++){
    //       sXsY += vj[k] * vi[(ni-h) + (k-w)];
    //     }
    //     double corw = ((h * sXsY) - (sXj * sYi)) / (sqrt((h * sXsqrdj)-(sXj * sXj)) * sqrt((h * sYsqrdi)-(sYi * sYi)));
    //     //Rprintf("%.4f ",corw);
    //     if(corw >= cutoff){
    //       ends[cutoffCounter] = w;
    //       cutoffCounter += 1;
    //     }
    //   }
    //   //Rprintf("\n");
    // }
  }
  return out;
}

//TODO: need to write this
int makeSx(List l,int h){
  return 0;
}

/*** R
# set.seed(42)
# library(tsdataleaks)
# a = round(rnorm(15),2)
# lst <- list(
#   a = a,
#   b = c(a[10:15],round(rnorm(10),2), a[1:5], a[1:5]),
#   c = c(round(rnorm(10),2), a[1:5])
# )
# lst
# tsdataleaks::find_dataleaks(lst,h = 5,cutoff = 1)
# find_dataleaks_cpp(lst,h = 5,cutoff = 1)
*/


