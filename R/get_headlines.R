
#' get_headlines
#'
#' Returns headlines from news sources.
#'
#' @param source Name of news source. 
#' @param category Name of category you want headlines for
#' @param country Name of country
#' @param keyword Name potential keyword                
#' @param api_key Character string API token. 
#' @param pageSize Number of hits per request 
#' @param page Number of page per request
#' @examples
#' \dontrun{
#' df <- get_headlines(api_key, "bbc-news")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom tidyr unnest
#' @name %>%
#' @rdname pipe
#' @return Data frame 
#' @export

get_headlines <- function(api_key, 
                          sources  = NULL, 
                          country  = NULL, 
                          category = NULL,
                          keyword  = NULL,
                          pageSize = 100,
                          page     = 1){
  
  
  # errors and warnings -----------------------------------------------------
  
  # check for api-key in the global environment
  if (is.null(api_key)) {
    api_key <- Sys.getenv("NEWS_API_KEY")
  }
  
  # is an api-key available?
  if (nchar(api_key) == 0){
    stop("Please provide an api-key with the function call or to the global
          environment")
  }

  # are any keywords available?
  if (all(sapply(list(sources, country, category, keyword), is.null))) {
    stop("Please provide at least one search term, i.e., source, country,
          category, or keyword.")
  }
  
  # provide access to searchterms, but hide this from the global environment
  data(searchterms, envir=environment())
  
  # are the provided arguments for category valid?
  if(!is.null(category)){
    if (!category %in% searchterms$category) {
      stop(paste0("Please provide a valid searchterm for category, such as:\n",
                   paste0(searchterms$category, collapse = ", ")))
    }
  }
  
  # are the provided arguments for country valid?
  if(!is.null(country)){
    if (!category %in% searchterms$country) { 
      stop(paste0("Please provide a valid searchterm for country, such as:\n",
                   paste0(searchterms$country, collapse = ", ")))
    }
  }     
   
  # are the provided arguments for sources valid? (trunctuates listing of 
  # news-sources - ideas?)
  if(!is.null(sources)){
    if (!sources %in% searchterms[['sources']]$results_df[['id']]) { 
      stop(paste0("Please provide a valid searchterm for news-sources.",
                  "Available newspapers are listed below: \n",
                  paste0(searchterms[['sources']]$results_df[['id']], 
                         collapse = ", ")))
    }
  }    
  
  # is the combination of search terms allowed?
  if (!is.null(sources) & (!is.null(country) | !is.null(category))){
    warning(paste0("'sources' must not be used as a search category with 
                    'country' and/or 'category'. Only 'sources' was used. 
                     Further arguments were ignored."))
    country <- NULL; category <- NULL
  }
  
  # more than one searchterm for country, category, or keyword?
  if (length(country) > 1 | length(category) > 1 | length(keyword) > 1){
    warning(paste0("Only one searchterm can be used for 'category' or",
                   "'country', or 'keyword'. Thus, only the first element was", 
                   "used. Further elements were ignored."))
    category = category[1]; country = country[1]; keyword = keyword[1]
  }    
  

  # access newsapi.org ------------------------------------------------------
  
  # multiple terms for sources? then paste strings together
  if (length(sources > 1)){
    sources <- paste(sources, collapse = ",")
  }
  
  # define url for query
  url <- httr::parse_url("https://newsapi.org/v2/top-headlines")
  url$scheme <- "https"
  url$query <- list(category = category,
                    sources  = sources,
                    country  = country,
                    q        = keyword,
                    pageSize = pageSize,
                    page     = page)
  
  # build url for query
  url  <- httr::build_url(url)
  # get results from query to newsapi
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  
  # build df from results ---------------------------------------------------
  
  # extract content
  content_text   <- httr::content(results, "text")
  content_parsed <- jsonlite::fromJSON(content)

  # create results data frame
  results_df        <- content_parsed$articles
  results_df$id     <- unlist(results_df$source$id)
  results_df$name   <- unlist(results_df$source$name)
  results_df$source <- NULL
  
  #change col 'publishedAt' from character to POSIX
  results_df$publishedAt <- as.POSIXct(results_df$publishedAt,
                                       tz = "UTC",
                                       format("%Y-%m-%dT%H:%M:%OSZ"))

  
  # build meta-data ---------------------------------------------------------
  
  metadata <- data.frame(total_results = content_parsed$totalResults, 
                         status_code   = results$status_code)

  
  # return results ----------------------------------------------------------
  return(list(metadata = metadata, results_df = results_df))
}
