context("Get headlines")
EXPECTED_METADATA_COLUMNS <- c("total_results", "status_code", "request_date", "request_url", 
                               "page_size", "page", "code", "message")
testthat::setup({
  EXPECTED_METADATA_COLUMNS <- c("total_results", "status_code", "request_date", "request_url", 
                                 "page_size", "page", "code", "message")
})

# TEST INVALID INPUTS

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_headlines())
})

testthat::test_that("that that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY")))
})

testthat::test_that("that that function returns error if invalid country is provided.", {
  testthat::expect_error(newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"),
                                                   query = c("sports"),
                                                   country = "HHH"))
})

testthat::test_that("that that function returns error if more than one country is provided.", {
  testthat::expect_warning(newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"),
                                                   query = c("sports"),
                                                   country = c("de", "fr")))
})

testthat::test_that("that that function raises warning vector is provided to query.", {
  testthat::expect_warning(newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"),
                                                   query = c("trump", "macron")))
})


testthat::test_that("that that function raises warning if list is provided to query.", {
  testthat::expect_warning(newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"),
                                                   query = list("trump", "macron")))
})


# INVALID API KEY

testthat::test_that("that that function returns error if API key invalid.", {
  testthat::expect_warning(newsanchor::get_headlines(api_key = "thisisnotanapikey",
                                                   query = c("sports")))
})


testthat::test_that("test that the it returns results", {
  res <- newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                   query = "sports")
  testthat::expect_gt(nrow(res$results_df), 0)
})

testthat::test_that("test that the function returns a data frame", {
  res <- newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                 query = "sports")
  testthat::expect_true(is.data.frame(res$results_df))
  testthat::expect_equal(names(res$metadata), EXPECTED_METADATA_COLUMNS, info = names(res$metadata))
})

testthat::test_that("test that all columns are atomic vectors", {
  res <- newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                 query = "sports")
  testthat::expect_true(all(sapply(res$results_df, is.atomic)),
                        info = paste0(colnames(res$results_df), " ", sapply(res$results_df, class)[!sapply(res$results_df, is.atomic)]))
})

testthat::test_that("test that the it returns empty dataframe with right columns", {
  res <- newsanchor::get_headlines(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                   query = "sports")
  testthat::expect_gt(nrow(res$results_df), 0)
})