test_that("corw detects perfect correlation", {
  y <- c(2,3,5,6)
  x <- c(13,14,15,y,24,25,27)
  expect_equal(round(corw(x,y),4),
               c(NA,NA,NA,
                 -0.6332,-0.9191,-0.5815,
                 1.0,0.8055,0.9640,0.8055))
})
