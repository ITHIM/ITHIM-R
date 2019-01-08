context("test-ithimr")

test_that("accra works", {
  # load saved result
  accra_results <- readRDS('accra_results.Rds')
  # generate new baseline accra results
  ithim_object <- run_ithim_setup()
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  # organise results to compare to 0. retain vector names.
  test_vals_1 <- abs(accra_results - result_mat)
  names(test_vals_1) <- names(accra_results)
  test_vals_1[accra_results>0] <- test_vals_1[accra_results>0]/accra_results[accra_results>0]
  test_vals_2 <- rep(0,length(result_mat))
  names(test_vals_2) <- names(result_mat)
  # test
  expect_equal(test_vals_1,test_vals_2,tolerance=0.05)
  
})
