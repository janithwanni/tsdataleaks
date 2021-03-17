library(microbenchmark)
Rcpp::sourceCpp('experiments/cpp_rowsums.cpp')
dyn.load("experiments/matsums.so")

M <- matrix(as.numeric(1:1E5),ncol = 5,byrow = TRUE)
v <- 1:5
# t(t(M) * v)
# M * outer(rep.int(1L, nrow(M)), v)
# M * v

microbenchmark(t.default(t.default(M) * v),t(t(M) * v),
               M * outer(rep.int(1L, nrow(M)), v),
               M * rep(v,each=nrow(M)), times = 100)

rowSums_zheyuan <- function (A, chunk.size, s, t) {

  # if (s == 1 && t == 1) result <- .Call("rowSums_1x1", A, as.integer(chunk.size))
  # if (s == 2 && t == 1) result <- .Call("rowSums_2x1", A, as.integer(chunk.size))
  # if (s == 1 && t == 2) result <- .Call("rowSums_1x2", A, as.integer(chunk.size))
  # if (s == 2 && t == 2)
  result <- .Call("rowSums_2x2", A, as.integer(chunk.size))
  # dyn.unload("matsums.so")
  result
}

fastRowSums <- function(mat){
  rowSums_zheyuan(mat,1360,2,2)
}


M <- matrix(as.numeric(1:1E6),ncol = 5,byrow = TRUE)
library(magrittr)
microbenchmark(
  rowSums(M),
  Cpp_rowSums(M),
  fastRowSums(M),
  rowSums_zheyuan(M,1360,2,2),
  rowSums_zheyuan(M,680,2,2),times = 500) %>% ggplot2::autoplot()

