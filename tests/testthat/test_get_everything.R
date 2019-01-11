context("Get everything")

testthat::setup({
  EXPECTED_METADATA_COLUMNS <- c("total_results", "status_code", "request_date", "request_url", 
                                 "page_size", "page", "code", "message")
  date_raw <- Sys.Date()
  DATE_BEGIN <- as.character(date_raw - as.difftime(2, unit= "days"))
  DATE_END <- as.character(date_raw)
  LIMIT_SOURCES <- 20
  PAGE_SIZE_LIMIT <- 100
})

# INVALID INPUTS
testthat::test_that("test that function returns error if no argument provided", {
  testthat::expect_error(newsanchor::get_everything())
})

testthat::test_that("test that function returns error if source limit is exceeded.", {
  testthat::expect_error(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                    query = "sports",
                                                    sources = rep("testsource", LIMIT_SOURCES + 5)))
})

testthat::test_that("test that function returns error if page size limit is exceeded.", {
  testthat::expect_error(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                    query = "sports", 
                                                    sources = "testsource",
                                                    page_size = 101
                                                    ))
})

testthat::test_that("test that function returns error if page size is not numeric.", {
  testthat::expect_error(newsanchor::get_everything(query = "sports", 
                                                    sources = "testsource",
                                                    page_size = "101"
  ))
})

testthat::test_that("test that function returns error if page is not numeric.", {
  testthat::expect_error(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                    query = "sports", 
                                                    sources = "testsource",
                                                    page_size = 10,
                                                    page = "9"
  ))
})

testthat::test_that("test that function returns error if non-existing language is specified.", {
  testthat::expect_error(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                    query = "sports", 
                                                    sources = "testsource",
                                                    page_size = 10,
                                                    page = 9,
                                                    language = "DOESNOTEXIST"
  ))
})

testthat::test_that("test that function returns error if attempting to sort by a non-existing sort key.", {
  testthat::expect_error(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                    query = "sports", 
                                                    sources = "testsource",
                                                    page_size = 10,
                                                    page = 9,
                                                    language = "de",
                                                    sort_by = "DOESNOTEXIST"
  ))
})

# INVALID API KEY
testthat::test_that("test that function raises warning if API key invalid.", {
  testthat::expect_warning(newsanchor::get_everything(api_key = "thisisnotanapikey",
                                                     query = "sports"))
})

# FORMAT OF RESULT DATA FRAME
testthat::test_that("test that a data frame is returned in the result list.", {
  testthat::skip_if(Sys.getenv("NEWS_API_TEST_KEY") == "", 
                    message = "NEWS_API_TEST_KEY not available in environment. Skipping test.")
  res <- newsanchor::get_everything(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                    from = DATE_BEGIN, to = DATE_END, 
                                    query = "Merkel")
  testthat::expect_true(is.data.frame(res$results_df), info = paste0(str(res)))
})

testthat::test_that("test that the correct number of rows is returned", {
  testthat::skip_if(Sys.getenv("NEWS_API_TEST_KEY") == "", 
                    message = "NEWS_API_TEST_KEY not available in environment. Skipping test.")
  res <- newsanchor::get_everything(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                    from = DATE_BEGIN, to = DATE_END, 
                                    query = "sports")
  testthat::expect_equal(nrow(res$results_df), 100, 
                         info = paste0("100 rows expected, but only ", nrow(res$results_df), " there."))
})

testthat::test_that("test that all columns are atomic vectors", {
  testthat::skip_if(Sys.getenv("NEWS_API_TEST_KEY") == "", 
                    message = "NEWS_API_TEST_KEY not available in environment. Skipping test.")
  res <- newsanchor::get_everything(api_key = Sys.getenv("NEWS_API_TEST_KEY"), 
                                    from = DATE_BEGIN, to = DATE_END, query = "sports")
  testthat::expect_true(all(sapply(res$results_df, is.atomic)), 
                        info = paste0(colnames(res$results_df)[!sapply(res$results_df, is.atomic)]))
})
