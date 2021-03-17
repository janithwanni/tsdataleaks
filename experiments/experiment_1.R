# # Benchmarks
# # Commented out due to the download size of M4comp2018 and the M4 dataset.
library(M4comp2018)
library(tsdataleaks)
library(profvis)
library(microbenchmark)
data(M4)
M4D <- Filter(function(l) l$period == "Daily", M4)
rm(M4)
M4D_x <- lapply(M4D, function(temp){temp$x})
n <- length(M4D_x)
if (is.null(names(M4D_x)) == TRUE){names(M4D_x) <- 1:n}
# Rcpp::sourceCpp("experiments/serial_get_endp.cpp",verbose = T)
# profvis({
#   m4d_f1 <- get_endpoints(M4D_x, h=14, cutoff = 1)
# })
# microbenchmark(
#   get_endpoints(M4D_x,h = 14,cutoff = 1),
# unit = "relative")
