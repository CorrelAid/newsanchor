#' Returns selected sources from newsapi.org
#' 
#' \code{get_sources} returns the news sources currently available on newsapi.org. 
#' The sources can be filtered using category, language or country. If the arguments are empty
#' the query return all available sources.
#'
#' @param category any category you might want to explore. Must be character or string
#' @param language the language of the newsoutlet (e.g. "de" or "en").
#' @param country the newsoutlet's country (e.g. "us").
#' @param api_key Character string with the API key you get from newsapi.org. 
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
  
  # is an api-key available?
  if (nchar(api_key) == 0){
    stop(paste0("You did not correctly specify your API key neither as global",
                " variable nor with the function call. See documentation for",
                " further info."))
  }
  
  # are the arguments for 'category' valid?
  if(!is.null(category)){
    if (!category %in% newsanchor::terms_category$category) {
      stop(paste0("Please provide a valid searchterm for category,", 
                  "see terms_category"))
    }
  }
  
  # check for a valid language:
  if(!is.null(language)){
    if (!language %in% newsanchor::terms_language$language) {
      stop(paste0("Please provide valid searchterm for language,",
                  "see terms_language"))
    } }
  
  # check for a valid country:
  if(!is.null(country)){
    if (!country %in%  newsanchor::terms_country$country) {
      stop(paste0("Please provide valid searchterm for country",
                  "see terms_country"))
    } }
  
  # access newsapi.org ------------------------------------------------------
  
  # define url for query
  url <- httr::parse_url("https://newsapi.org/v2/sources")
  url$scheme <- "https"
  url$query <- list(category = category,
                    language = language,
                    country  = country)
  # build url for query
  url <- httr::build_url(url)
  # get results from query to newsapi
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  
  # build df from results ---------------------------------------------------
  # extract content
  content_text   <- httr::content(results, "text")
  content_parsed <- jsonlite::fromJSON(content_text)
  
  # check whether content_parsed is NULL 
  if(length(content_parsed$sources) < 1){
    content_parsed$totalResults <- 0
  } else {
    content_parsed$totalResults <- nrow(content_parsed$sources)
  }
  
  #--- Check if http status code equals 200 (and construct results accordingly)
  if (results$status_code == 200 & content_parsed$totalResults != 0 ) {
    
    # create results data frame
    results_df        <- content_parsed$sources
    
    # extract meta-data
    metadata <- data.frame(total_results = nrow(results_df), 
                           status_code   = results$status_code,
                           request_date  = results$date,
                           request_url   = results$url,
                           code          = results$status_code,
                           message       = content_parsed$status,
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
                           code          = ifelse(results$status_code != 200, 
                                                  content_parsed$code, 
                                                  results$status_code),
                           message       = ifelse(results$status_code != 200,
                                                  content_parsed$message, 
                                                  content_parsed$status),
                           stringsAsFactors = FALSE)
  }
  
  # return results ----------------------------------------------------------
  return(list(metadata = metadata, results_df = results_df))
}


