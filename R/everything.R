#' Get all resources that the API of newsapi.org provides
#'
#' @param api_key Character string with the API key you get from newsapi.org. Passing it is compulsory. Function gets it per default from your global environment. See "set-api-key" for more info.
#' @param content Character string that contains the content of your search in the API data base
#' @param nrtexts The number (numeric!) of articles per page that are returned. Maximum is 100, default is 20.
#' @param sources Character string with IDs (comma separated) of the news outlets you want to focus on (e.g., "usa-today, spiegel-online").
#' @param domains Character string (comma separated) with domains that you want to restrict your search to (e.g. "bbc.com, nytimes.com").
#' @param exclude_domains Similar usage as 'domains'. Will exclude these domains from your search.
#' @param from Marks the date of the start of your search. Must be in ISO 8601 format (e.g., '2018-09-08' or '2018-09-08T12:51:42'). Default is the oldest.
#' @param to Marks the end date of your search. Works similarly as 'from'. Default is the latest article.
#' @param language Specifies the language of the articles of your search. Must be in ISO shortcut format (e.g., "de", "en"). See list of all languages on https://newsapi.org/docs/endpoints/everything. Default is all languages.
#' @param sort_by Character string that specifies the sorting of your article results. Has three options: "publishedAt", "relevancy", "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results. Must be numeric. Default is first page.
#' @return A list containing a data frame with results for your search and another list with metadata on your search.
#' @examples
#' get_everything(api_key = key, content = "stuttgart", language = "de")
#' get_everything(api_key = key, content = "mannheim", from = "2018-01-07T12:00:00")
#'

apiKey = "acf81c97c91746c2bc491a1110c1f0ad" 

Sys.setenv("NEWS_API_KEY" = apiKey)

get_everything <- function(api_key = Sys.getenv("NEWS_API_KEY"),
                           content,
                           nr_texts = 20, 
                           sources = NULL,
                           domains = NULL,
                           exclude_domains = NULL,
                           from = NULL, 
                           to = NULL,
                           language = NULL, 
                           sort_by = "publishedAt", 
                           page = NULL) {
  
  # Check evaluation of passed arguments
  sortings <- c("publishedAt", "relevancy", "popularity")
  
  ISO <- c("ar", "de", "en", "es", "fr", "he", "it", "nl", "no",
           "pt", "ru", "se", "ud", "zh")
  
  # Make sure that the API key is passed
  #if(missing(api_key) == TRUE)
    #stop("You need to pass your API key.")
  
  if(is.null(api_key) == TRUE)
    stop("You did not correctly specify your API key as global variable. See documentation for further info.")
  
  # Make sure that some content is passed
  if(missing(content) == TRUE)
    stop("You need to specify at least some content that you search for.")
  
  # Make sure page size does not exceed 100
  if(!is.numeric(nr_texts)) {
    
    stop("You need to insert numeric values for the number of texts per page.")
    
  } else if(nr_texts > 100) {
    
    stop("Page size cannot exceed 100 articles.")
    
  }
  
  if(!is.null(page)) {
    stopifnot(is.numeric(page))#, call. =  FALSE)
    #stop("Arguments 'page' and 'nrtexts' need to be numeric")
  }
  
  if(!is.null(language)) {
    stopifnot((sum(language == ISO) == 1))#, call. = FALSE) # language
    #stop("Language must be specified as one of these ISO codes: ar, de, en, es, fr, he, it, nl, no, pt, ru, se, ud, zh)
  }
  
  if(!sort_by %in% c("publishedAt", "relevancy", "popularity")){
    stop("Sortings can be only by 'publishedAt', 'relevancy', or 'popularity'.")
  }

  # Build URL
  rawurl <- httr::parse_url("https://newsapi.org/v2/everything")
  
  rawurl$query <- list(q = content,
                       pageSize = nr_texts,
                       page = page,
                       language = language,
                       sources = sources,
                       domains = domains,
                       excludeDomains = exclude_domains,
                       from = from,
                       to = to,
                       sortBy = sort_by)
  
  url <- httr::build_url(rawurl)
  
  # make request & parse result
  res <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  content_text <- httr::content(res, "text")
  content_parsed <- jsonlite::fromJSON(content_text)

  # create results data frame
  results_df <- content_parsed$articles
  results_df$id <- unlist(results_df$source$id)
  results_df$name <- unlist(results_df$source$name)
  results_df$source <- NULL

  results_df$publishedAt <- as.POSIXct(results_df$publishedAt,
                                       tz = "UTC",
                                       format("%Y-%m-%dT%H:%M:%OSZ"))
  
  # Create metadata list
  metadata <- list(total_results = content_parsed$totalResults,
                   status_code = res$status_code,
                   request_date = res$date,
                   request_url = res$url)
  return(list(metadata = metadata, results_df = results_df))
  
}

