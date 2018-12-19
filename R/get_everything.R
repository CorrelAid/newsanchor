#' Get all resources of newsapi.org
#' 
#' \code{get_everything} searches through articles from large and small news 
#' sources and blogs. This includes breaking news as well as other regular articles.
#' You  can search for multiple \code{sources}, different \code{language}, 
#' or use your own keywords. Articles can be sorted by the earliest date 
#' \code{publishedAt}, \code{relevancy}, or \code{popularity}. To automatically 
#' download all results, use \code{get_everything_all()}\cr\cr
#' Please check that the \code{api_key} is available. You can provide an explicit
#' definition of the api_key or use \code{set_api_key()}. \cr\cr
#' Valid languages for \code{language} are provided in the dataset \cr\cr
#' \code{terms_language}. 
#' 
#' @param query Character string that contains the searchterm for the API's 
#'              data base. API supports advanced search parameters, see 'details'. 
#' @param sources Character string with IDs (comma separated) of the news outlets 
#'                you want to focus on (e.g., "usa-today, spiegel-online").
#' @param domains Character string (comma separated) with domains that you want 
#'                to restrict your search to (e.g., "bbc.com, nytimes.com").
#' @param exclude_domains Similar usage as with 'domains'. Will exclude these 
#'                        domains from your search.
#' @param from Marks the start date of your search. Must be in ISO 8601 format 
#'             (e.g., "2018-09-08" or "2018-09-08T12:51:42"). Default is the 
#'             oldest available date (depends on your paid/unpaid plan from 
#'             newsapi.org).
#' @param to Marks the end date of your search. Works similarly to 'from'. 
#'           Default is the latest article available.
#' @param language Specifies the language of the articles of your search. Must 
#'                 be in ISO shortcut format (e.g., "de", "en"). See list of all 
#'                 languages on https://newsapi.org/docs/endpoints/everything. 
#'                 Default is all languages.
#' @param sort_by Character string that specifies the sorting of your article 
#'                results. Accepts three options: "publishedAt", "relevancy", 
#'                "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results that is returned. Must 
#'             be numeric. Default is first page. If you want to get all results 
#'             at once, use \code{get_everything_all} from 'newsanchor'.
#' @param page_size The number of articles per page that are returned. 
#'                  Maximum is 100 (also default).
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key}).
#' 
#' @details Advanced search (see also www.newsapi.org): Surround entire phrases 
#'          with quotes (") for exact matches. Prepend words/phrases that must 
#'          appear with "+" symbol (e.g., +bitcoin). Prepend words that must not 
#'          appear with "-" symbol (e.g., -bitcoin). You can also use AND, OR, 
#'          NOT keywords (optionally grouped with parenthesis, e.g., 'crypto AND 
#'          (ethereum OR litecoin) NOT bitcoin)').
#' @examples
#' \dontrun{
#' df <- get_everything(query = "stuttgart", language = "de")
#' df <- get_everything(query = "mannheim", from = "2019-01-02T12:00:00")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom jsonlite fromJSON
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' @export

get_everything <- function(query,
                           sources         = NULL,
                           domains         = NULL,
                           exclude_domains = NULL,
                           from            = NULL, 
                           to              = NULL,
                           language        = NULL, 
                           sort_by         = "publishedAt", 
                           page            = 1,
                           page_size       = 100, 
                           api_key = Sys.getenv("NEWS_API_KEY")) {
  
  # Initial proceedings -----------------------------------------------------
  
  # Provide a vector with available ways of sorting the articles  
  sortings <- c("publishedAt", "relevancy", "popularity")
  
  # Errors and warnings -----------------------------------------------------
  
  # Make sure an API key is provided
  if(nchar(api_key) == 0)
    stop(paste0("You did not correctly specify your API key as global variable.", 
                " See documentation for further info."))
  
  # Make sure that some search term is passed
  if(missing(query) == TRUE)
    stop("You need to specify at least some content that you search for.")
  
  # Bind together various search parameters as comma-separated strings as required by API
  
  # Parameter: sources (plus limit to maximum of 20 sources)
  if(length(sources) > 20) {
    stop("You cannot select more than 20 sources in one search request.")
  } else {
  if(length(sources) > 1) {
  sources <- paste(sources, collapse = ",")
    }
  }
  
  # Parameter: domains 
  if(length(domains) > 1) {
    domains <- paste(domains, collapse = ",")
  }
  
  # Parameter: exclude_domains
  if(length(exclude_domains) > 1) {
    exclude_domains <- paste(exclude_domains, collapse = ",")
  }
  
  # check that page_size is <= 100
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

  # Error if language indicated does not match the ones provided by the API
  if(!is.null(language)){
    if(!language %in% newsanchor::terms_language$language) {
      stop(paste0("Please provide a valid search terms for language ", 
                  "see terms_language"))
    }
  }
  
  # Error if selected sorting does not match the ones provided by the API
  if(!sort_by %in% c("publishedAt", "relevancy", "popularity")){
    stop("Sortings can be only by 'publishedAt', 'relevancy', or 'popularity'.")
  }
  
  # Accessing the API  -----------------------------------------------------
  
  # Build URL
  rawurl <- httr::parse_url("https://newsapi.org/v2/everything")
  
  rawurl$query <- list(q               = query,
                       language        = language,
                       sources         = sources,
                       domains         = domains,
                       excludeDomains  = exclude_domains,
                       from            = from,
                       to              = to,
                       sortBy          = sort_by,
                       pageSize        = page_size,
                       page            = page)
  
  # build url for query
  url <- httr::build_url(rawurl)
  
  # get results from query to newsapi
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))

  
  # build df from results ---------------------------------------------------
  
  # extract content
  content_text    <- httr::content(results, "text")
  content_parsed  <- jsonlite::fromJSON(content_text)
  
  
  # check whether content_parsed is NULL 
  if(is.null(content_parsed$totalResults)){
    content_parsed$totalResults <- 0
  }
  
  #--- Check if http status code equals 200 (and construct results accordingly)
  if (results$status_code == 200 & content_parsed$totalResults != 0) {
    
    # create results data frame
    results_df          <- content_parsed$articles
    results_df$id       <- unlist(results_df$source$id)
    results_df$name     <- unlist(results_df$source$name)
    results_df$source   <- NULL
    
    # Rename two columns with camelCase to snake_case
    names(results_df)[names(results_df) == 'publishedAt'] <- 'published_at'
    names(results_df)[names(results_df) == 'urlToImage'] <- 'url_to_image'
    
    # change col 'published_at' from character to POSIX (if available)
    if(!is.null(results$published_at)){
      results_df$published_at <- as.POSIXct(results_df$published_at,
                                            tz = "UTC")
    }
    
    # Create metadata list
    metadata <- data.frame(total_results  = content_parsed$totalResults,
                           status_code    = results$status_code,
                           request_date   = results$date,
                           request_url    = as.character(results$url),
                           page_size      = page_size,
                           page           = page,
                           code           = "",
                           message        = "",
                           stringsAsFactors = F)

  }
    
  #--- if the http code displays an error, throw a warning and build the results 
  #     accordingly
  if (results$status_code != 200 | content_parsed$totalResults == 0) {
    
    # provide warning for error message
    if(results$status_code != 200) {
      warning(paste0("The search resulted in the following error message:" , 
                     content_parsed$message))
    }
    
    # provide warning that zero results (but only if status_code == 200)
    if (results$status_code == 200){
      if (content_parsed$totalResults == 0){
        warning(paste0("The search was not successful. There were no results",
                       " for your specifications."))
      }
    }
    
    # Return empty data.frame
    results_df = data.frame()
    
    # Extract meta data 
    metadata <- data.frame(total_results = 0, 
                           status_code   = results$status_code,
                           request_date  = results$date,
                           request_url   = results$url,
                           page_size     = page_size,
                           page          = page,
                           code          = ifelse(results$status_code != 200, 
                                                  content_parsed$code, 
                                                  ""),
                           message       = ifelse(results$status_code != 200, 
                                                  content_parsed$message, 
                                                  ""),
                           stringsAsFactors = F)
  }
  

  # return results ----------------------------------------------------------
  return(list(metadata    = metadata, 
              results_df  = results_df))
  
}

