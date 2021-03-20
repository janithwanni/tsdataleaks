Rcpp::sourceCpp('experiments/split_list.cpp')
Rcpp::sourceCpp('experiments/cpp_rowsums.cpp')
dyn.load("experiments/matsums.so")

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

build_endpoints <- function(lstx,h,cutoff){
  profvis::profvis({
  # system.time({
  # microbenchmark::microbenchmark({
  n <- length(lstx)
  lslens <- unlist(lapply(lstx,length))
  lsnames <- names(lstx)
  L <- sum(lslens) - (n * h) + n
  data <- split_list(lstx,h,L) # matrix X (L x h)
  data <- t.default(t.default(data) - (fastRowSums(data) / h)) # matrix X - Xm (L x h)
  data2 <- t.default(fastRowSums(data * data))
  Ycounter <- cumsum(lslens - h + 1)
  Y <- data[Ycounter,] # matrix n x h
  Y <- t.default(t.default(Y) - (fastRowSums(Y) / h))
  Y2 <- fastRowSums(Y * Y)
  data <- t.default(data)
  Rlist <- list()
  Rlist <- get_corr_list(data,data2,Y,Y2,lslens,lsnames)
  # for(i in 1:n){
  #   nl <- lslens[i]
  #   b <- Y[i,]
  #   a <- t.default(data * b)
  #   r_n <- fastRowSums(a)
  #   c <- t.default(data2 * Y2[i])
  #   r_d <- sqrt(c)
  #   r <- r_n / r_d
  #   # r_num <- fastRowSums(data * (Y[i,]))
  #   # r_den <- sqrt(data2 * sum(Y2[i,]))
  #   # namel <- lsnames[i]
  #   # Rlist[[namel]] <- r_num / r_den
  # }
  })
  # Rlist
}


x <- rnorm(1E2)
y <- rnorm(1E2)
x <- c(x, y)
ls <- list(a = x,b = y)
build_endpoints(ls,4,1)
# system.time({build_endpoints(ls, 4, 1)})
library(M4comp2018)
M4D <- Filter(function(l) l$period == "Daily", M4)[1:500]
M4D_x <- lapply(M4D, function(temp){temp$x})
n <- length(M4D_x)
if (is.null(names(M4D_x)) == TRUE){names(M4D_x) <- 1:n}

build_endpoints(M4D_x,14,1)
# z <- rnorm(5)
# ls <- list(a = x,b = y,c = z)
# build_endpoints(ls, 4, 1)
