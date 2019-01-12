#' Builds query URL for newsapi.org.
#'
#' \code{build_newsanchor_url} adds a list of query arguments to a given 
#' News API endpoint.
#' 
#' @param url NEWS API endpoint.
#' @param query_params named list of parameters that are needed to query the endpoint. 
#' Check the News API documentation to see which endpoint requires which parameters.
#'
#' @importFrom httr parse_url 
#' @importFrom utils data
#' 
#' @return httr URL. 
build_newsanchor_url <- function(url, query_args){
  url <- httr::parse_url(url = url)
  url$scheme <- "https"
  url$query <- query_args
  return(url)
}

#' Makes a GET request to News API. 
#'
#' \code{make_newsanchor_get_request} makes a GET request to News API. 
#' 
#' @param url News API url with query parameters and scheme specified. See build_newsanchor_url. 
#' @param api_key News API key.
#'
#' @importFrom httr GET 
#' @importFrom utils data
#' 
#' @return httr response object. 
make_newsanchor_get_request <- function(url, api_key){
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  return(results)
}

#' Parses content returned by query to the News API.
#'
#' \code{parse_newsanchor_content} parses the content sent back by
#' the News API to an R list.
#' 
#' @param response httr response object
#' 
#' @importFrom httr content 
#' @importFrom utils data
#' @importFrom jsonlite fromJSON
#' 
#' @return R list. 
parse_newsanchor_content <- function(response){
  content_text    <- httr::content(response, "text")
  content_parsed  <- jsonlite::fromJSON(content_text)
  return(content_parsed)
}

#' Extracts metadata. 
#'
#' \code{extract_newsanchor_metadata} extracts meta data from the response object and the 
#' parsed content.
#' 
#' @param response httr response object
#' @param content_parsed parsed content of a response to News API query
#' @param page Specifies the page number of your results that was returned. Defaults to NULL.
#' @param page_size The number of articles per page that were returned. Defaults to NULL.
#' 
#' @return data frame containing meta data related to the query.
extract_newsanchor_metadata <- function(response, content_parsed, total_results, page = NULL, page_size = NULL){
  
  metadata <- data.frame(total_results = content_parsed$totalResults, 
                         status_code   = response$status_code,
                         request_date  = response$date,
                         request_url   = response$url,
                         page_size     = page_size,
                         page          = page,
                         code          = ifelse(response$status_code !=200, 
                                                content_parsed$code, 
                                                ""),
                         message       = ifelse(response$status_code !=200,
                                                content_parsed$message, 
                                                ""),
                         stringsAsFactors = FALSE)
  return(metadata)
}

#' Extracts data frame with News API sources from response object. 
#'
#' \code{extract_newsanchor_sources} extracts a data frame containing the News API sources that
#' matched the request to News API sources endpoint.
#' 
#' @param metadata data frame containing meta data related to the request, see extract_newsanchor_metadata.
#' @param content_parsed parsed content of a response to News API query
#' 
#' @return data frame containing sources.
extract_newsanchor_sources <- function(metadata, content_parsed){
  empty_df <- data.frame()
  if (response$status_code == 200) {
    if(metadata$total_results > 0){
      return(content_parsed$sources)
    } else {
      warning(paste0("The search was not successful. There were no results",
                     " for your specifications."))
      return(empty_df)
    }
  } else {
    # an error occurred
      warning(paste0("The search resulted in the following error message: ",
                     metadata$message))
    return(empty_df)
  }
}

#' Extracts data frame with News API articles from response object.
#'
#' \code{extract_newsanchor_articles} extracts a data frame containing the News API articles that
#' matched the request to News API everything or headlines endpoint.
#' 
#' @param metadata data frame containing meta data related to the request, see extract_newsanchor_metadata.
#' @param content_parsed parsed content of a response to News API query
#' 
#' @return data frame containing articles.
extract_newsanchor_articles <- function(metadata, content_parsed){
  empty_df <- data.frame()
  
  if (metadata$status_code == 200) {
    if(metadata$total_results > 0){
      results_df        <- content_parsed$articles
      results_df$id     <- unlist(results_df$source$id)
      results_df$name   <- unlist(results_df$source$name)
      results_df$source <- NULL
      
      # rename two columns with camelcase to snake_case
      names(results_df)[names(results_df) == 'publishedAt'] <- 'published_at'
      names(results_df)[names(results_df) == 'urlToImage'] <- 'url_to_image'
      
      # change col 'published_at' from character to POSIX (if available)
      if(!is.null(results_df$published_at)){
        results_df$published_at <- as.POSIXct(results_df$published_at,
                                              tz = "UTC",
                                              format("%Y-%m-%dT%H:%M:%OSZ"))
      }
    } else {
      warning(paste0("The search was not successful. There were no results",
                     " for your specifications."))
      return(empty_df)      
    }
  } else {
    warning(paste0("The search resulted in the following error message:" , 
                   metadata$message))
    return(empty_df)
  }
}

#' Concatenate character vector to comma-separated string.
#'
#' \code{collapse_to_comma_separated} is a helper function that concatenates a character vector 
#' to a comma-separated string. If the input vector has only one element, the element will be returned unchanged.
#' 
#' @param v character vector.
#' @examples 
#' collapse_to_comma_separated("foo") # "foo"
#' collapse_to_comma_separated(c("foo", "bar")) # "foo, bar"
#' @return string with elements of v separated by comma.
#' 
collapse_to_comma_separated <- function(v){
  return(paste(v, collapse = ","))
}

#' Checks validity of a category.
#'
#' \code{stop_if_invalid_category} checks whether a given category is valid for News API and 
#' stops with an error if this is not the case.
#' 
#' @param category category to check as a string.
#'
stop_if_invalid_category <- function(category){
  if (!category %in% newsanchor::terms_category$category) {
    stop(paste0(category, " is not a valid category. ",
                "See terms_category for a list of valid categories."))
  }
}

#' Checks validity of a country
#'
#' \code{stop_if_invalid_country} checks whether a given country is valid for News API and 
#' stops with an error if this is not the case.
#' 
#' @param country country to check as a string.
#'
stop_if_invalid_country <- function(country){
  if (!country %in%  newsanchor::terms_country$country) {
    stop(paste0(country, " is not a valid country. ",
                "See terms_country for a list of valid countries."))
  }
}

#' Checks validity of a source
#'
#' \code{stop_if_invalid_source} checks whether a given source is valid for News API and 
#' stops with an error if this is not the case.
#' 
#' @param source source to check as a string.
#'
stop_if_invalid_source <- function(source){
  if (!source %in% newsanchor::terms_sources$sources) { 
    stop(paste0(source, "is not a valid source name. ",
                "See terms_sources for a list of valid sources."))
  }
}

#' Checks validity of a language
#'
#' \code{stop_if_invalid_language} checks whether a given language is valid for News API and 
#' stops with an error if this is not the case.
#' 
#' @param language language to check as a string.
#'
stop_if_invalid_language <- function(language){
  if(!language %in% newsanchor::terms_language$language) {
    stop(paste0("Language ", language, " is not a valid language, ", 
                "see data frame terms_language for valid languages."))
  }
}