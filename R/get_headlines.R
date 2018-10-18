#' Returns headlines provided by the API from newsapi.org
#'
#' \code{get_headlines} returns live top and breaking headlines for a country, 
#' specific category in a country, single source, or multiple sources. You can 
#' also search with keywords. Articles are sorted by the earliest date 
#' published first.\cr\cr
#' Please run \code{set_api_key} first. Alternatively, provide an explicit
#' definition of the api_key. \cr\cr
#' For valid searchterms see \code{data(searchterms)}
#' 
#' @param query Keyword you want headlines for
#' @param category Category you want headlines from
#' @param country Country you want headlines for
#' @param sources Sources you want headlines from
#' @param page Use this to page through the results, if the total results 
#'             found is greater than the \code{page_size}. 
#' @param page_size Number of hits per request (maximum = 100)
#' @param api_key Your API token 
#' 
#' @examples
#' \dontrun{
#' df <- get_headlines(sources = "bbc-news")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' @export

get_headlines <- function(query     = NULL, 
                          category  = NULL,
                          country   = NULL, 
                          sources   = NULL, 
                          page      = 1,
                          page_size = 100,
                          api_key   = Sys.getenv("NEWS_API_KEY")){
  
  
  # errors and warnings -----------------------------------------------------
  
  # is an api-key available?
  if (nchar(api_key) == 0){
    stop("Please provide an api-key")
  }

  # are any querys available?
  if (all(sapply(list(sources, country, category, query), is.null))) {
    stop(paste0("Please provide at least one search term, i.e., query,",
                "category, country, or source."))
  }
  
  # provide access to searchterms, but hide this from the global environment
  data(terms_category, terms_country, terms_sources, envir=environment())
  
  # are the arguments for 'category' valid?
  if(!is.null(category)){
    if (!category %in% terms_category$category) {
      stop(paste0("Please provide a valid searchterm for category,", 
                  "see data(terms_category)"))
    }
  }
  
  # are the arguments for 'country' valid?
  if(!is.null(country)){
    if (!category %in% terms_country$country) { 
      stop(paste0("Please provide a valid searchterm for country,", 
                  " see data(terms_country)"))
    }
  }     
   
  # are the arguments for 'sources' valid? 
  if(!is.null(sources)){
    if (!sources %in% terms_sources$id) { 
      stop(paste0("Please provide a valid searchterm for news-sources,",
                  " see data(terms_sources)."))
    }
  }    
  
  # is the combination of search terms allowed?
  if (!is.null(sources) & (!is.null(country) | !is.null(category))){
    warning(paste0("'sources' must not be used as a search category with ",
                   "'country' and/or 'category'. Only 'sources' was used.",
                   " Further arguments were ignored."))
    country <- NULL; category <- NULL
  }

  # more than one searchterm for 'country', 'category', or 'query'?
  if (length(country) > 1 | length(category) > 1 | length(query) > 1){
   warning(paste0("Only one searchterm can be used for 'category' or",
                  "'country', or 'query'. Thus, only the first element was",
                  " used. Further elements were ignored."))
   category = category[1]; country = country[1]; query = query[1]
  }
  
  # check that page_size is <= 100
  if(!is.numeric(page_size)) {
    stop("Page_size needs to be a number.")
  } 
  else if(page_size > 100) {
    stop("Page size cannot exceed 100 articles per page.")
  }
  
  
  
  # access newsapi.org ------------------------------------------------------
  
  # multiple terms for sources? then paste strings together
  if (length(sources > 1)){
    sources <- paste(sources, collapse = ",")
  }

  # define url for query
  url <- httr::parse_url("https://newsapi.org/v2/top-headlines")
  url$scheme <- "https"
  url$query <- list(category  = category,
                    sources   = sources,
                    country   = country,
                    q         = query,
                    pageSize  = page_size,
                    page      = page)
  
  # build url for query
  url  <- httr::build_url(url)
  # get results from query to newsapi
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  
  # build df from results ---------------------------------------------------
  
  # extract content
  content_text   <- httr::content(results, "text")
  content_parsed <- jsonlite::fromJSON(content_text)

  # create results data frame
  results_df        <- content_parsed$articles
  results_df$id     <- unlist(results_df$source$id)
  results_df$name   <- unlist(results_df$source$name)
  results_df$source <- NULL
  
  # rename two columns with camelcase to snake_case
  names(results_df)[names(results_df) == 'publishedAt'] <- 'published_at'
  names(results_df)[names(results_df) == 'urlToImage'] <- 'url_to_image'
  
  # change col 'published_at' from character to POSIX
  results_df$published_at <- as.POSIXct(results_df$published_at,
                                       tz = "UTC",
                                       format("%Y-%m-%dT%H:%M:%OSZ"))

  # build meta-data ---------------------------------------------------------
  
  metadata <- data.frame(total_results = content_parsed$totalResults, 
                         status_code   = results$status_code,
                         request_data  = results$date,
                         request_url   = results$url)

  # return results ----------------------------------------------------------
  return(list(metadata = metadata, results_df = results_df))
}
