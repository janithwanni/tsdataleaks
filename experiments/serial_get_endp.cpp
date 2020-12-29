#include <Rcpp.h>
using namespace Rcpp;

NumericVector corw(NumericVector x,NumericVector y){
  // nx > ny
  int ny = y.size();
  int nx = x.size();
  // Rprintf("nx %d ny %d\n",nx,ny);
  NumericVector cor(nx);
  for(int i = 0;i < ny-1;i++){
    cor[i] = NA_REAL;
  }

  bool fresh = false;
  double sY = sum(y);
  double sYSq = sum((y*y));
  double K = sqrt((ny*sYSq)-(sY*sY));
  double sX;
  double sXSq;
  double old_x_one;
  for(int start = 0;start < (nx - ny+1);start++){
    NumericVector x_slice = x[Rcpp::Range(start,start+ny)];
    //Rprintf("slice start %d slice end %d\n",start,(start+ny));

    if(!fresh){
      double sumXman = 0;
      double sumXSqman = 0;
      for(int i = 0;i<ny;i++){
        //Rprintf("x[%d] = %.4f;\n",i,x_slice[i]);
        sumXman += x_slice[i];
        sumXSqman += x_slice[i] * x_slice[i];
      }
      old_x_one = x_slice[0];
      sX = sumXman;
      sXSq = sumXSqman;
      fresh = true;
    }else{
      sX = sX - old_x_one + x_slice[ny-1];
      sXSq = sXSq - (old_x_one * old_x_one) + (x_slice[ny-1]*x_slice[ny-1]);
      old_x_one = x_slice[0];
    }
    //Rprintf("\n\n xold = %.4f;sX = %.4f;sXSq = %.4f\n\n",old_x_one,sX,sXSq);
    double sumXYman = 0;
    for(int i = 0;i<ny;i++){
      sumXYman += x_slice[i] * y[i];
    }
    double sXsY = sumXYman;
    double denom = sqrt((ny*sXSq) - (sX*sX)) * K;
    //Rprintf("denom %.4f sY %.4f sYSq %.4f sXsY %.4f\n",denom,sY,sYSq,sXsY);
    double partA = (ny * sXsY) / denom;
    double partB = (sX * sY) / denom;
    double cor_val = partA - partB;
    //Rprintf("cor_val %.4f\n",cor_val);
    cor[start+ny-1] = cor_val;
  }
  return cor;
}

// [[Rcpp::export]]
List get_endpoints(List l,int h = 5,double cutoff = 1){
  List out;
  CharacterVector lnames = l.names();
  for(int i = 0;i<l.size();i++){
    List endps;
    NumericVector li = l[i];
    NumericVector Y = li[Range(li.size() - h,li.size() - 1)];
    for(int k = 0;k<l.size();k++){
      NumericVector X = l[k];
      NumericVector corr = corw(X,Y);
      endps.push_back(corr,as<std::string>(lnames[k]));
    }
    out.push_back(endps,as<std::string>(lnames[i]));
  }
  return out;
}

#include <RcppParallel.h>
using namespace RcppParallel;

struct corWorker : public Worker{
  const Rcpp::List input;

  Rcpp::List output;

  List l;
  int h;
  CharacterVector lnames;
  corWorker(const Rcpp::List input,
            List output,
            List l,int h) : input(input), output(output),l(l),h(h) {
    lnames = l.names();
  }
  void operator()(std::size_t begin, std::size_t end) {
    for(std::size_t i = begin;i != end; i++){
      NumericVector li = input[i];
      NumericVector Y = li[Range(li.size() - h,li.size() - 1)];
      Rcout << Y << std::endl;
      //series.push_back(endps,as<std::string>(lnames[i]));
    }
    //output.push_back(NULL);
  }
};


void parallelget_endp(List l,int h = 5,int cutoff = 1) {


  RVector<double> output(10);
  // SquareRoot functor (pass input and output matrixes)
  corWorker corworker(l, output,h,cutoff);

  // call parallelFor to do the work
  parallelFor(0, l.size(), corworker);

  // return the output matrix
}
/*** R
set.seed(42)
# library(tsdataleaks)
a = round(rnorm(15),2)
lst <- list(
  a = a,
  b = c(a[10:15],round(rnorm(10),2), a[1:5], a[1:5]),
  c = c(round(rnorm(10),2), a[1:5])
)
lst
tsdataleaks::find_dataleaks(lst,h = 5,cutoff = 1)
get_endpoints(lst)
*/
