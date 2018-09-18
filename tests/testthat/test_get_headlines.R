context("get headlines")

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_headlines())
})

testthat::test_that("test that the number of rows is greater than zero", {
  res <- newsanchor::get_headlines(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                   keyword = "sports")
  testthat::expect_gt(nrow(res), 0)
})

testthat::test_that("test that the function returns a data frame", {
  res <- newsanchor::get_headlines(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 keyword = "sports")
  testthat::expect_s3_class(res, "data.frame")
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_headlines(api_key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 keyword = "sports")
  testthat::expect_true(all(sapply(res, is.atomic)))
})