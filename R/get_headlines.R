#' Returns selected headlines from newsapi.org
#'
#' \code{get_headlines} returns live top and breaking headlines for a country, 
#' specific category in a country, single source, or multiple sources. You can 
#' also search with keywords. Articles are sorted by the earliest date 
#' published first. To automatically download all results, use 
#' \code{get_headlines_all}\cr\cr
#' Please check that the api_key is available. You can provide an explicit
#' definition of the api_key or use \code{set_api_key} \cr\cr
#' Valid searchterms are provided in \code{terms_category}, 
#' \code{terms_country} or \code{terms_sources}
#' 
#' 
#' @param query Character string that contains the searchterm 
#' @param category Character string with the category you want headlines from
#' @param country Character string with the country you want headlines from
#' @param sources Character string with IDs (comma separated) of the news outlets 
#'                you want to focus on (e.g., "usa-today, spiegel-online").
#' @param page Specifies the page number of your results that is returned. Must 
#'             be numeric. Default is first page. If you want to get all results 
#'             at once, use \code{get_headlines_all} from 'newsanchor'.
#' @param page_size The number of articles per page that are returned. 
#'                  Maximum is 100 (also default).
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key}).
#' 
#' @examples
#' \dontrun{
#' df <- get_headlines(sources = "bbc-news")
#' df <- get_headlines(query = "sports", page = 2)
#' df <- get_headlines(category = "business")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @importFrom utils data
#' 
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
    stop(paste0("You did not correctly specify your API key neither as global",
                " variable nor with the function call. See documentation for",
                " further info."))
  }

  # are any searchterms provided?
  if (all(sapply(list(sources, country, category, query), is.null))) {
    stop(paste0("Please provide at least one searchterm"))
  }
  
  # are the arguments for 'category' valid?
  if(!is.null(category)){
    if (!category %in% newsanchor::terms_category$category) {
      stop(paste0("Please provide a valid searchterm for category,", 
                  "see terms_category"))
    }
  }
  
  # are the arguments for 'country' valid?
  if(!is.null(country)){
    if (!country %in% newsanchor::terms_country$country) { 
      stop(paste0("Please provide a valid searchterm for country,", 
                  " see terms_country"))
    }
  }     
   
  # are the arguments for 'sources' valid? 
  if(!is.null(sources)){
    if (!sources %in% newsanchor::terms_sources$sources) { 
      stop(paste0("Please provide a valid searchterm for news-sources,",
                  " see terms_sources."))
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

  # check that page_size is numeric and <= 100
  if(!is.numeric(page_size)) {
    stop("You need to insert numeric values for the number of texts per page.")
  } 
  else if(page_size > 100) {
    stop("Page size cannot not exceed 100 articles per page.")
  }
  
  # Error for non-numeric page parameter
  if(!is.numeric(page)) {
    stop("Page should be a number.")
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
  
  # # check whether content_parsed is NULL 
  if(is.null(content_parsed$totalResults)){
    content_parsed$totalResults <- 0
  }


  #--- Check if http status code equals 200 (and construct results accordingly)
  if (results$status_code == 200 & content_parsed$totalResults != 0 ) 
    {
    
    # create results data frame
    results_df        <- content_parsed$articles
    results_df$id     <- unlist(results_df$source$id)
    results_df$name   <- unlist(results_df$source$name)
    results_df$source <- NULL
    
    # rename two columns with camelcase to snake_case
    names(results_df)[names(results_df) == 'publishedAt'] <- 'published_at'
    names(results_df)[names(results_df) == 'urlToImage'] <- 'url_to_image'
    
    # change col 'published_at' from character to POSIX (if available)
    if(!is.null(results$published_at)){
      results_df$published_at <- as.POSIXct(results_df$published_at,
                                            tz = "UTC")
    }

    # extract meta-data
    metadata <- data.frame(total_results = content_parsed$totalResults, 
                           status_code   = results$status_code,
                           request_date  = results$date,
                           request_url   = results$url,
                           page_size     = page_size,
                           page          = page,
                           code          = "",
                           message       = "",
                           stringsAsFactors = FALSE)
  }

  #--- if the http code displays an error, throw a warning and build the results 
  #    accordingly
  if (results$status_code != 200 | content_parsed$totalResults == 0) {
    
    # provide warning for error message
    if (results$status_code != 200){
      warning(paste0("The search resulted in the following error message: ",
                      content_parsed$message))
    }

    # provide warning that zero results (but only if status_code == 200)
    if (results$status_code == 200){
      if (content_parsed$totalResults == 0){
        warning(paste0("The search was not successful. There were no results",
                       " for your specifications."))
      }
    }
    
    # empty results dataframe
    results_df = data.frame()
    # extract meta-data
    metadata <- data.frame(total_results = 0, 
                           status_code   = results$status_code,
                           request_date  = results$date,
                           request_url   = results$url,
                           page_size     = page_size,
                           page          = page,
                           code          = ifelse(results$status_code !=200, 
                                                  content_parsed$code, 
                                                  ""),
                           message       = ifelse(results$status_code !=200,
                                                  content_parsed$message, 
                                                  ""),
                           stringsAsFactors = FALSE)
  }
  

  # return results ----------------------------------------------------------
  
  return(list(metadata = metadata, results_df = results_df))
}
