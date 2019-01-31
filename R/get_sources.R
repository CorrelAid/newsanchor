#' Returns selected sources from newsapi.org
#' 
#' \code{get_sources} returns the news sources currently available on newsapi.org. 
#' The sources can be filtered using category, language or country. If the arguments are empty
#' the query return all available sources.
#'
#' @param category Category you want to get sources for as a string. Default: NULL.
#' @param language The langauge you want to get sources for as a string. Default: NULL.
#' @param country The country you want to get sources for as a string (e.g. "us"). Default: NULL.
#' @param api_key  String with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key}).
#' 
#' @importFrom httr content GET build_url parse_url add_headers
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' 
#' @examples 
#' \dontrun{
#' get_sources(api_key)
#' get_sources(api_key, category = "technology")
#' get_sources(api_key, language = "en")
#' }
#' 
#' @export

get_sources <- function(category = NULL,
                        language = NULL,
                        country = NULL,
                        api_key   = Sys.getenv("NEWS_API_KEY")) {
  
  # errors and warnings -----------------------------------------------------
  
  # are the arguments for 'category' valid?
  if(!is.null(category)){
    if(length(category) > 1){
      stop("You cannot specify more than one category.")
    }
    stop_if_invalid_category(category)
  }
  
  # check for a valid language:
  if(!is.null(language)){
    if(length(language) > 1){
      stop("You cannot specify more than one language.")
    }
    stop_if_invalid_language(language)
  } 
  
  # check for a valid country:
  if(!is.null(country)){
    if(length(country) > 1){
      stop("You cannot specify more than one country.")
    }
    stop_if_invalid_country(country)
  }
  
  # is an api-key available?
  if (nchar(api_key) == 0){
    stop(paste0("You did not correctly specify your API key neither as global",
                " variable nor with the function call. See documentation for",
                " further info."))
  }
  
  # access newsapi.org ------------------------------------------------------
  query_params <- list(category = category,
                       language = language,
                       country  = country)
  url <- build_newsanchor_url("https://newsapi.org/v2/sources", query_params)

  response <- make_newsanchor_get_request(url, api_key)

  # build df from results ---------------------------------------------------
  content_parsed <- parse_newsanchor_content(response)
  # check whether content_parsed is NULL 
  if(is.null(content_parsed$sources)){
    content_parsed$totalResults <- 0
  } else {
    content_parsed$totalResults <- nrow(content_parsed$sources)
  }
  
  metadata <- extract_newsanchor_metadata(response, content_parsed)
  results_df <- extract_newsanchor_sources(metadata, content_parsed)
  
  # return results ----------------------------------------------------------
  return(list(metadata = metadata, results_df = results_df))
}


