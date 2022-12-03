x<- c(1,1.3,1.6,1,2.1,1,0.8,-1,-1.2)
y<- c(NA, 0.3000000,  0.2307692, -0.375,  1.1, -0.5238095, -0.2, -2.25, 0.2)

testthat::expect_equal(transf.pctChange(x,1),y,tolerance = 0.0001)

