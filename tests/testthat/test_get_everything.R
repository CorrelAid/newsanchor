context("Get everything")
testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_everything())
})


testthat::test_that("test that a data frame is returned in the result list.", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = "2018-09-10", to = "2018-09-12", 
                                    content = "Merkel")
  testthat::expect_true(is.data.frame(res$results_df), info = paste0(str(res)))
})

testthat::test_that("test that the correct number of rows is returned", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = "2018-09-10", to = "2018-09-12", 
                                    content = "Merkel")
  testthat::expect_equal(nrow(res$results_df), 20, 
                         info = paste0("20 rows expected, but only ", nrow(res$results_df), " there."))
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = "2018-09-10", to = "2018-09-12", content = "Merkel")
  testthat::expect_true(all(sapply(res$results_df, is.atomic)), 
                        info = paste0(colnames(res$results_df)[!sapply(res$results_df, is.atomic)]))
})