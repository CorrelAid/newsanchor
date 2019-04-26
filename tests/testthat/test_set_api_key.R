context("test set_api_key")

tmp <- tempfile()
api_key <- "foobar"

testthat::test_that("test that the set api key function creates a file with the api key", {
  
  # mock out the askpass function because we do not want interactivity in the tests
  mockery::stub(newsanchor::set_api_key, "askpass::askpass", api_key)
  
  newsanchor::set_api_key(path = tmp)
  
  # test that the temp file exists
  testthat::expect_true(file.exists(tmp))
  content <- readLines(tmp) # read content
  testthat::expect_true(content == paste0("NEWS_API_KEY=", api_key))
})

testthat::teardown({
  file.remove(tmp)
})