#' Returns selected headlines from newsapi.org
#'
#' \code{get_headlines} returns live top and breaking headlines for a country, 
#' specific category in a country, single source, or multiple sources. You can 
#' also search with keywords. Articles are sorted by the earliest date 
#' published first. To automatically download all results, use 
#' \code{get_headlines_all()}.\cr\cr
#' Please check that the \code{api_key} is available. You can provide an explicit
#' definition of the key or use \code{set_api_key()}. \cr\cr
#' Valid searchterms are provided in the data sets \code{terms_category}, 
#' \code{terms_country} or \code{terms_sources}.
#' 
#' 
#' @param query Character string that contains the searchterm. 
#' @param category Character string with the category you want headlines from.
#' @param country Character string with the country you want headlines from.
#' @param sources Character vector with with IDs of the news outlets 
#'                you want to focus on (e.g., c("usa-today", "spiegel-online")).
#' @param page Specifies the page number of your results that is returned. Must 
#'             be numeric. Default is first page. If you want to get all results 
#'             at once, use \code{get_headlines_all} from 'newsanchor'.
#' @param page_size The number of articles per page that are returned. 
#'                  Maximum is 100 (also default).
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, a function can be 
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

  # are any search terms provided?
  if (all(sapply(list(sources, country, category, query), is.null))) {
    stop(paste0("Please provide at least either sources, country, category, or query."))
  }
  
  if(!is.null(query)){
    if(length(query) > 1){
      stop("You can only specify one query string.")
    }
  }
  
  # is the combination of search terms allowed?
  if (!is.null(sources) & (!is.null(country) | !is.null(category))){
    stop(paste0("'sources' cannot be used together with ",
                "'country' and/or 'category'."))
  }
  
  # are the arguments for 'category' valid?
  if(!is.null(category)){
    if(length(category) > 1){
      stop("You cannot specify more than one category.")
    }
    stop_if_invalid_category(category)
  }
  
  # are the arguments for 'country' valid?
  if(!is.null(country)){
    if(length(country) > 1){
      stop("You cannot specify more than one country")
    }    
    stop_if_invalid_country(country) 
  }     
   
  # are the arguments for 'sources' valid? 
  if(!is.null(sources)){
    sapply(sources, stop_if_invalid_source)
    sources <- collapse_to_comma_separated(sources)
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
  
  # is an api-key available?
  if (nchar(api_key) == 0){
    stop(paste0("You did not correctly specify your API key neither as global",
                " variable nor with the function call. See documentation for",
                " further info."))
  }
  
  # access newsapi.org ------------------------------------------------------
  # define url for query
  query_params <- list(category  = category,
                    sources   = sources,
                    country   = country,
                    q         = query,
                    pageSize  = page_size,
                    page      = page)
  url <- build_newsanchor_url("https://newsapi.org/v2/top-headlines", query_params)
  
  
  # get results from query to newsapi
  response <- make_newsanchor_get_request(url = url, api_key = api_key)
  
  # build return data from results ---------------------------------------------------
  content_parsed <- parse_newsanchor_content(response)
  if(is.null(content_parsed$totalResults)){
    content_parsed$totalResults <- 0
  }
  metadata <- extract_newsanchor_metadata(response, content_parsed, page, page_size)
  results_df = extract_newsanchor_articles(metadata = metadata, content_parsed = content_parsed)
  
  return(list(metadata = metadata, results_df = results_df))
}
