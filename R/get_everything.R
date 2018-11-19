#' Get all resources that the API of newsapi.org provides for a given searchterm
#'
#' @param api_key Character string with the API key you get from newsapi.org. Passing it is compulsory. Function obtains it per default from your global environment. See "set-api-key" for more info.
#' @param query Character string that contains the searchterm for the API's data base. API supports advanced search parameters, see 'details'. 
#' @param page_size The number (numeric!) of articles per page that are returned. Maximum is 100 (also default).
#' @param sources Character string with IDs (comma separated) of the news outlets you want to focus on (e.g., "usa-today, spiegel-online").
#' @param domains Character string (comma separated) with domains that you want to restrict your search to (e.g., "bbc.com, nytimes.com").
#' @param exclude_domains Similar usage as with 'domains'. Will exclude these domains from your search.
#' @param from Marks the start date of your search. Must be in ISO 8601 format (e.g., "2018-09-08" or "2018-09-08T12:51:42"). Default is the oldest available date (depends on your paid/unpaid plan from newsapi.org).
#' @param to Marks the end date of your search. Works similarly to 'from'. Default is the latest article available.
#' @param language Specifies the language of the articles of your search. Must be in ISO shortcut format (e.g., "de", "en"). See list of all languages on https://newsapi.org/docs/endpoints/everything. Default is all languages.
#' @param sort_by Character string that specifies the sorting of your article results. Accepts three options: "publishedAt", "relevancy", "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results that is returned. Must be numeric. Default is first page. If you want to get all results at once, use 'get_everything_all' from 'newsanchor'.
#' @return A list containing 1) a data frame with results for your search and 2) another list with metadata on your search.
#' @details Advanced search (see also www.newsapi.org): Surround entire phrases with quotes (") for exact matches. Prepend words/phrases that must appear with "+" symbol (e.g., +bitcoin). Prepend words that must not appear with "-" symbol (e.g., -bitcoin). You can also use AND, OR, NOT keywords (optionally grouped with parenthesis, e.g., 'crypto AND (ethereum OR litecoin) NOT bitcoin)').
#' @examples
#' \dontrun{
#' get_everything(api_key = key, content = "stuttgart", language = "de")
#' get_everything(api_key = key, content = "mannheim", from = "2018-01-07T12:00:00")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom jsonlite fromJSON
#' @export
#' 

get_everything <- function(api_key = Sys.getenv("NEWS_API_KEY"),
                           query,
                           page_size       = 100, 
                           sources         = NULL,
                           domains         = NULL,
                           exclude_domains = NULL,
                           from            = NULL, 
                           to              = NULL,
                           language        = NULL, 
                           sort_by         = "publishedAt", 
                           page            = NULL) {
  
  # Initial proceedings -----------------------------------------------------
  
  # Provide a vector with available ways of sorting the articles  
  sortings <- c("publishedAt", "relevancy", "popularity")
  
  # Errors and warnings -----------------------------------------------------
  
  # Make sure an API key is provided
  if(nchar(api_key) == 0)
    stop("You did not correctly specify your API key as global variable. See documentation for further info.")
  
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
  
  # Make sure page size does not exceed 100
  if(!is.numeric(page_size)) {
    
    stop("You need to insert numeric values for the number of texts per page.")
    
  } else if(page_size > 100) {
    
    # Error if page size exceeds maximum of 100 articles
    stop("Page size cannot exceed 100 articles.")
    
  }
  
  # Error for non-numeric page parameter
  if(!is.null(page)) {
    stopifnot(is.numeric(page))
  }
  
  # If no page number is specified, set it to value "1"
  if(is.null(page)) {page = 1}
  
  # Error if language indicated does not match the ones provided by the API
  if(!is.null(country)){
    if(!category %in% newsanchor::terms_country$everything) {
      stop(paste0("Please provide a valid country,", "see data(terms_country)"))
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
                       pageSize        = page_size,
                       page            = page,
                       language        = language,
                       sources         = sources,
                       domains         = domains,
                       excludeDomains  = exclude_domains,
                       from            = from,
                       to              = to,
                       sortBy          = sort_by)
  
  url <- httr::build_url(rawurl)
  
  # Make request & parse result
  results <- httr::GET(url, httr::add_headers("X-Api-Key" = api_key))
  
  content_text    <- httr::content(results, "text")
  content_parsed  <- jsonlite::fromJSON(content_text)
  
  # Check if http status code is valid and construct results accordingly
  if (results$status_code == 200 & content_parsed$totalResults != 0) {
    
  # Create results data frame
  results_df          <- content_parsed$articles
  results_df$id       <- unlist(results_df$source$id)
  results_df$name     <- unlist(results_df$source$name)
  results_df$source   <- NULL
  
  # Rename two columns with camelCase to snake_case
  names(results_df)[names(results_df) == 'publishedAt'] <- 'published_at'
  names(results_df)[names(results_df) == 'urlToImage'] <- 'url_to_image'
  
  # Convert publishing date to posixct format
  results_df$published_at <- as.POSIXct(results_df$published_at,
                                       tz = "UTC",
                                       format("%Y-%m-%dT%H:%M:%OSZ"))
  
  # Create metadata list
  metadata <- list(total_results  = content_parsed$totalResults,
                   status_code    = results$status_code,
                   request_date   = results$date,
                   request_url    = results$url,
                   page_size      = page_size,
                   page           = page,
                   code           = NA,
                   message        = NA)
  
  }
    
  # If the http code displays an error, throw a warning and build the results accordingly
  if (results$status_code != 200 | content_parsed$totalResults == 0) {
    
    if(results$status_code != 200) {
      warning(paste0("The search resulted in the following error message:" , content_parsed$message))
    }
    
    if(content_parsed$totalResults == 0) {
      warning(paste0("The search was not successful. There were no results for your specifications."))
    }
    
    # Return empty data.frame
    results_df = data.frame()
    
    # Extract meta data 
    metadata <- data.frame(total_results = 0, 
                           status_code   = results$status_code,
                           request_data  = results$date,
                           request_url   = results$url,
                           page_size     = NA,
                           page          = NA,
                           code          = ifelse(results$status_code != 200, content_parsed$code, NA),
                           message       = ifelse(results$status_code != 200, content_parsed$message, NA))
  }
  
  # Return results and metadata
  return(list(metadata    = metadata, 
              results_df  = results_df))
  
}
