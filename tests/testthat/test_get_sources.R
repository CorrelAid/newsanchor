context("Get sources")

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_sources(api_key = ""))
})

testthat::test_that("test that the function returns a data frame", {
  res <- newsanchor::get_sources(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 category = "general",
                                 language = "de")
  print(paste0( "structure ", str(res)))
  testthat::expect_true(is.data.frame(res$results_df), 
                        info = paste0(str(res)))
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_sources(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 category = "general",
                                 language = "de")
  testthat::expect_true(all(sapply(res$results_df, is.atomic)),
                        info = paste0(str(res$results_df)))
})

testthat::test_that("test that the number of rows is greater than zero", {
  res <- newsanchor::get_sources(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    category = "general",
                                    language = "de")
  testthat::expect_gt(nrow(res$results_df), 0)
})

