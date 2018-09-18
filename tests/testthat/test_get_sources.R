context("Get sources")

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_sources())
})

testthat::test_that("test that the number of rows is greater than zero", {
  res <- newsanchor::get_sources(apiKey = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    category = "sports",
                                    language = "de")
  testthat::expect_gt(nrow(res), 0)
})

testthat::test_that("test that the function returns a data frame", {
  res <- newsanchor::get_sources(apiKey = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 category = "sports",
                                 language = "de")
  testthat::expect_true(is.data.frame(res))
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_sources(apiKey = "457e40043f6b4418ab108e3eb11ccc1e", 
                                 category = "sports",
                                 language = "de")
  testthat::expect_true(all(sapply(res, is.atomic)))
})