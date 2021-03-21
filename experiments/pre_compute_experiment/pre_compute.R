Rcpp::sourceCpp('experiments/pre_compute_experiment/split_list.cpp')
# Rcpp::sourceCpp('experiments/pre_compute_experiment/cpp_rowsums.cpp')
# Rcpp::sourceCpp("experiments/pre_compute_experiment/get_precompute.cpp")

build_endpoints <- function(lstx,h,cutoff){
  profvis::profvis({
    n <- length(lstx)
    lslens <- unlist(lapply(lstx,length))
    lsnames <- names(lstx)
    L <- sum(lslens) - (n * h) + n
    X <- split_list(lstx,h,L) # matrix X (L x h)
    XsqS <- rowSums(X * X) # sigma(X^2)
    sX <- rowSums(X)
    sXsq <- sX * sX # sigma(X)^2
    denom_X <- (h * XsqS) - sXsq

    Ycounter <- cumsum(lslens - h + 1)

    prevCounter <- 1
    for(counter in Ycounter){
      X_vals <- X[prevCounter:counter,]
      y_val <- X[counter,]
      sXY <-  rowSums(t(t(X_vals) * y_val))
      indice <- 1
      for(Xcounter in prevCounter:counter){
        denom <- sqrt(denom_X[Xcounter] * denom_X[counter])
        num_left <- sXY[indice] * h
        indice <- indice + 1
        num_right <- sX[Xcounter] * sX[counter]
        numer <- num_left - num_right
        r = numer / denom
      }
      prevCounter <- counter + 1
    }
  })
}

lst <- list(
  a = 1:6,
  b = 7:11
)
# build_endpoints(lst,3,1)
library(M4comp2018)
data(M4)
M4D <- Filter(function(l) l$period == "Daily", M4)
rm(M4)
M4D_x <- lapply(M4D, function(temp){temp$x})
build_endpoints(M4D_x,14,1)
