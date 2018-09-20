#' Get (currently) available sources from newsapi.org.
#'
#' @param api_key your API key - it is required to make the API call.
#' @param category any category you might want to explore. Must be character or string
#' @param language the language of the newsoutlet (e.g. "de" or "en").
#' @param country the newsoutlet's country (e.g. "us").
#' @importFrom httr content GET build_url parse_url add_headers
#' @return A dataframe based on your specifications
#' @examples
#' get_sources(api_key)
#' get_sources(api_key, category="technology")
#' get_sources(api_key, language="en")

get_sources <- function(api_key = NULL,
                        category = NULL,
                        language = NULL,
                        country = NULL) {
  
  if (is.null(api_key)) {
    
    api_key <- Sys.getenv("NEWS_API_KEY")
  
  }
  
  
  force(api_key)
  
  # check for a valid category:
  if(!is.null(category)){
    if (!category %in% c("business", "entertainment", "general" ,"health" ,
                         "science", "sports", "technology")) {
      stop("Your input category is invalid. For a list of valid categories see:
                                   https://newsapi.org/docs/endpoints/sources")
    } }
  
  # check for a valid language:
  if(!is.null(language)){
    if (!language %in% c("ar", "de", "en", "es", "fr", "he",
                         "it", "nl", "no", "pt", "ru", "se", "ud", "zh")) {
      stop("Your input language is invalid. For a list of valid categories see:
                                   https://newsapi.org/docs/endpoints/sources")
    } }
  # check for a valid country:
  if(!is.null(country)){
    if (!country %in%  c("ae", "ar" ,"at","au", "be" ,"bg", "br" ,"ca",
                         "ch", "cn", "co", "cu", "cz", "de", "eg", "fr",
                         "gb", "gr", "hk", "hu", "id", "ie", "il",  "in",
                         "it", "jp", "kr", "lt", "lv", "ma", "mx", "my",
                         "ng", "nl", "no", "nz", "ph", "pl", "pt", "ro",
                         "rs", "ru", "sa", "se", "sg", "si", "sk", "th",
                         "tr", "tw", "ua", "us", "ve", "za")) {
      stop("Your input country is invalid. For a list of valid categories see:
                                 https://newsapi.org/docs/endpoints/sources")
    } }
  
  
  
  
  
  # build and parse url
  url <- httr::parse_url("https://newsapi.org/v2/sources")
  url$scheme <- "https"
  url$query <- list(
    category = category,
    language = language,
    country = country,
    apiKey = api_key)
  url <- httr::build_url(url)
  # API call
  
  result <- httr::GET(url)
  content <- httr::content(result, "text")
  prep_data <- jsonlite::fromJSON(content)
  
  # return result in a dataframe
  if ("sources" %in% names(prep_data)) { # just to be safe
    data <- prep_data$sources
  }

  return(list(metadata = list(), results_df = data))
  
}


