
#' get_headlines
#'
#' Returns headlines from news sources.
#'
#' @param source Name of news source. 
#' @param category Name of category you want headlines for. Options include
#'                 'business', 'entertainment', 'general', 'health', 'science'
#'                 'sports', 'technology'
#' @param country Name of country
#' @param keyword Name potential keyword                
#' @param api_key Character string API token. 
#' @examples
#' \dontrun{
#' df <- get_headlines(api_key, "bbc-news")
#' }
#' @importFrom httr content GET build_url add_headers
#' @importFrom tidyr %>% unnest
#' @name %>%
#' @rdname pipe
#' @return Data frame 
#' @export

get_headlines <- function(api_key, 
                          source   = NULL, 
                          country  = NULL, 
                          category = NULL,
                          keyword  = NULL){
  
  
  #------------------------- ERRORS and WARNINGS ----------------------------#
  
  # no apikey provided
  if (missing("api_key")){
    stop("Please provide the api-key.")
  }
  
  # no arguments at all provided
  if (all(sapply(list(source, country, category, keyword), is.null))) {
    stop("Please provide at least one search term.")
  }
  
  
  # arguments for 'category' are not indexed
  if(!is.null(category)){
    if (!category %in% c("business", "entertainment", "general" ,"health" ,
                         "science", "sports", "technology")) {
      stop(paste0("Please provide a valid searchterm for category, see ",
                  "https://newsapi.org/docs/endpoints/top-headlines"))
    }
  }
  
  # arguments for 'country' are not indexed
  if(!is.null(country)){
    if (!country %in% c("ae", "ar", "at", "au", "be", "bg", "br", "ca", "ch", 
                        "cn", "cn", "co", "cu", "cz", "de", "eg", "fr", "gb", 
                        "gr", "hk", "hu", "id", "ie", "il", "in", "it", "jp", 
                        "kr", "lt", "lv", "ma", "mx", "my", "ng", "nl", "no", 
                        "nz", "ph", "pl", "pt", "ro", "rs", "ru", "sa", "se", 
                        "sg", "si", "sk", "th", "tr", "tw", "ua", "us", "ve", 
                        "za")) {
      stop(paste0("Please provide a valid searchterm for country, see ",
                  "https://newsapi.org/docs/endpoints/top-headlines"))
    }
  }     
    
  # incongruent combination of search terms
  if (!is.null(source) & (!is.null(country) | !is.null(category))){
    print(paste0("warning: 'sources' must not be used as a search category",
                 "with 'country' and/or 'category'. Only 'sources' was used.",
                 "Further arguments were ignored."))
    country <- NULL; category <- NULL
  }
  
  # only one searchterm for country, category, keyword is allowed
  if (length(country) > 1 | length(category) > 1 | length(keyword) > 1){
    print(paste0("warning: Only one searchterm can be used for 'category' or",
                 "'country', or 'keyword'. Thus, only the first element was", 
                 "used. Further elemnts were ignored."))
    category = category[1]; country = country[1]; keyword = keyword[1]
  }    
  

  
  #--------------------------- ACCESS NEWSAPI.ORG -----------------------------#
  
  # multiple terms for sources? then paste strings together
  if (length(source > 1)){
    source <- paste(source, collapse = ",")
  }
  
  # create list for creation of url
  list_for_url <- list()
  list_for_url[["scheme"]]         <- "https"
  list_for_url[["hostname"]]       <- "newsapi.org"
  list_for_url[["path"]]           <- "v2/top-headlines"
  list_for_url[["query"]]$sources  <- source
  list_for_url[["query"]]$country  <- country
  list_for_url[["query"]]$category <- category
  list_for_url[["query"]]$q        <- keyword
  list_for_url[["query"]]$pageSize <- 100
  class(list_for_url) <- "url"
  
  # build url for query
  url  <- httr::build_url(list_for_url)
  # get results from query to newsapi
  results_list <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  # print status response
  print(c("status response from query: ", results_list$status_code))
  # extract content
  results_list <- httr::content(results_list)
  # total results
  print(c("number of total results: ", results_list$totalResults))
  total_results <- results_list$totalResults
  
  #--------------------- CONVERT RESULTS TO DATAFRAME -----------------------#
  
  # change list to tibble
  results_df <- as.data.frame(do.call("rbind", results_list$articles))
  # extract id from list in source
  results_df$id <- results_df %>% tidyr::unnest(source) %>%
    as.data.frame() %>% .[c(TRUE, FALSE), ]
  # extract name from list in source
  results_df$name <- results_df %>% tidyr::unnest(source) %>%
    as.data.frame() %>% .[c(FALSE, TRUE), ]
  # delete old source column
  results_df$source <- NULL
  
}