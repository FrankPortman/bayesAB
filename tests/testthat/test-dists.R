library(bayesAB)
context('dists')

test_that("Failures based on inputs", {
  
  expect_error(plotPoisson(1, 5), 'area must be in (0, 1)', fixed = TRUE)
  
  expect_error(plotPareto(1, 1, 5), 'area must be in (0, 1)', fixed = TRUE)
  
  expect_error(plotGamma(1, 1, 5), 'area must be in (0, 1)', fixed = TRUE)
  
  expect_error(plotInvGamma(1, 1, 5), 'area must be in (0, 1)', fixed = TRUE)
  
  expect_error(plotLogNormal(1, 1, 5), 'area must be in (0, 1)', fixed = TRUE)
  
  expect_error(dinvgamma(5, -1, 5), "Shape or scale parameter negative")

})

test_that("Success", {
  
  expect_equal(plotPoisson(1)$labels$y, 'PDF')
  expect_equal(plotPareto(1, 1)$labels$y, 'PDF')
  expect_equal(plotNormal(1, 1)$labels$y, 'PDF')
  expect_equal(plotGamma(1, 1)$labels$y, 'PDF')
  expect_equal(plotBeta(1, 1)$labels$y, 'PDF')
  expect_equal(plotInvGamma(1, 1)$labels$y, 'PDF')
  expect_equal(plotLogNormal(1, 1)$labels$y, 'PDF')
  expect_equal(qinvgamma(1 - (.Machine$double.eps) / 2, 2, 2), Inf)
    
})
