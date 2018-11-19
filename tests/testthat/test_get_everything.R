context("Get everything")

date_raw <- Sys.Date()
date1 <- as.character(date_raw - as.difftime(2, unit= "days"))
date2 <- as.character(date_raw)

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_everything())
})


testthat::test_that("test that a data frame is returned in the result list.", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = date1, to = date2, 
                                    query = "Merkel")
  testthat::expect_true(is.data.frame(res$results_df), info = paste0(str(res)))
})

testthat::test_that("test that the correct number of rows is returned", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = date1, to = date2, 
                                    query = "Merkel")
  testthat::expect_equal(nrow(res$results_df), 100, 
                         info = paste0("100 rows expected, but only ", nrow(res$results_df), " there."))
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_everything(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = date1, to = date2, query = "Merkel")
  testthat::expect_true(all(sapply(res$results_df, is.atomic)), 
                        info = paste0(colnames(res$results_df)[!sapply(res$results_df, is.atomic)]))
})
