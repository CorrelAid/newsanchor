context("Get everything")
testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_everything())
})

testthat::test_that("test that the correct number of rows is returned", {
  res <- newsanchor::get_everything(key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = "2018-09-10", to = "2018-09-12", 
                                    content = "Merkel")
  testthat::expect_equal(nrow(res$results_df), 20)
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_everything(key = "457e40043f6b4418ab108e3eb11ccc1e", 
                                    from = "2018-09-10", to = "2018-09-12", content = "Merkel")
  testthat::expect_true(all(sapply(res$articles, is.atomic)))
})