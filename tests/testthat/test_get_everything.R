context("foo")
testthat::test_that("get everything returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_everything())
})

testthat::test_that("test with api key", {
  res <- newsanchor::get_everything(key = "457e40043f6b4418ab108e3eb11ccc1e", content = "Merkel")
  testthat::expect_equal(nrow(res), 20)
})