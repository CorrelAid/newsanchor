#' Get (currently) available sources from newsapi.org.
#'
#' @param apiKey your API key - it is required to make the API call.
#' @param category any category you might want to explore. Must be character or string
#' @param language the language of the newsoutlet (e.g. "de" or "en").
#' @param country the newsoutlet's country (e.g. "us").
#' @importFrom httr content GET build_url parse_url add_headers
#' @return A dataframe based on your specifications
#' @examples
#' get_sources(api)
#' get_sources(api, category="technology")
#' get_sources(api, language="en")

get_sources <- function(apiKey = NULL,
                        category = NULL,
                        language = NULL,
                        country = NULL
){
  force(apiKey)
  
  # category Fehlermeldung:
  if(!is.null(category)){
    if (!category %in% c("business", "entertainment", "general" ,"health" ,
                         "science", "sports", "technology")) {
      stop("Your input category is invalid. For a list of valid categories see:
                                   https://newsapi.org/docs/endpoints/sources")
    } }
  
  # language Fehlermeldung
  if(!is.null(language)){
    if (!language %in% c("ar", "de", "en", "es", "fr", "he",
                         "it", "nl", "no", "pt", "ru", "se", "ud", "zh")) {
      stop("Your input language is invalid. For a list of valid categories see:
                                   https://newsapi.org/docs/endpoints/sources")
    } }
  # country fehlermeldung
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
  
  
  
  
  
  # die URL bauen mit den übergebenen parametern:
  url <- parse_url("https://newsapi.org/v2/sources")
  url$scheme <- "https"
  url$query <- list(
    category = category,
    language = language,
    country = country,
    apiKey = apiKey)
  url <- httr::build_url(url)
  # API call
  result <- httr::GET(url)
  result <- httr::content(result, "parsed")
  # das Ergebnis in einen Dataframe übergeben und returnen:
  if ("sources" %in% names(result)) { # nur zur Sicherheit...
    result <- result[["sources"]]
  }
  data <- data.frame(do.call("rbind", result)) # good old base r :D
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  return(data)
  
}





                      # die URL bauen mit den übergebenen parametern:
                      url <- httr::parse_url("https://newsapi.org/v2/sources")
                      url$scheme <- "https"
                      url$query <- list(
                                         category = category,
                                         language = language,
                                         country = country,
                                         apiKey = apiKey)
                      url <- httr::build_url(url)
                      # API call
                      result <- httr::GET(url)
                      result <- httr::content(result, "parsed")
                      # das Ergebnis in einen Dataframe übergeben und returnen:
                      if ("sources" %in% names(result)) { # nur zur Sicherheit...
                        result <- result[["sources"]]
                      }
                      data <- data.frame(do.call("rbind", result)) # good old base r :D
                      data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
                      return(data)

                          }

#api <- ""

#data2 <- get_sources(apiKey = api)
#str(data2)
#category <- "technology"
#language <- "de"
#country <- "de"

#data3 <- get_sources(api,category = category,language = language,country = country)
#str(data3)




